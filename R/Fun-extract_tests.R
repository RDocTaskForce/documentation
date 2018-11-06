{###############################################################################
# extract_tests.R
# This file is part of the R package `documentation`.
#
# Copyright 2017 Andrew Redd
# Date: 2017-06-09
#
# DESCRIPTION
# ===========
# Extract blocks for testing.
#
# LICENSE
# ========
# The R package `documentation` is free software:
# you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This software is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program. If not, see http://www.gnu.org/licenses/.
#
}###############################################################################
#' @include Classes.R

makeActiveBinding( '.tests.head.lines', function(){
    include.time <- getOption("documentation::include_extraction_time", FALSE)
    c( ._("#! This file was automatically produced by the documentation package.")
     , if(include.time) ._("#! Extracted on %s", Sys.time())
     , ._("#! Changes will be overwritten.")
     , ''
     )
}, topenv())

#@internal
extract_tests_to_file_ <-
function( file              #< file to extract tests from
        , file.out  = NULL  #< file to write tests to, if provided must be fully specified, ie. `dir` will be ignored.
        , test.dir  = NULL  #< directory where to store extracted blocks.
        , verbose   = default('verbose', FALSE) #< Show progress messages?
        , full.path = FALSE
        , force = FALSE     #< Force extraction
        ){
    doc_message(._("* Extracting tests from file `%s`.\n", file)) %if% verbose
    if (is.null(file.out)){
        if (is.null(test.dir)){
            test.dir <- '.'
            if (file.exists(. <- file.path(test.dir, "tests"   ))) test.dir <- .
            if (file.exists(. <- file.path(test.dir, "testthat"))) test.dir <- .
            if (verbose) message("  + `test.dir` not provided. Setting to `", test.dir, "`")
        }
        file.out <- file.path(test.dir, sprintf("test-%s", basename(file)))
    }

    if (!force && file.exists(file.out) && file.mtime(file) < file.mtime(file.out)){
        doc_message(._("  + file `%s` is newer.  SKIPPING.\n", file.out)) %if% verbose
        return(invisible(s(character(0), test.file=file.out)))
    }

    doc_message(._("  + Writting extracted tests to `%s`.", file.out)) %if% verbose
    #! Extract `if(F){#! @TESTTHAT }` blocks from file
    content <- parsetools::extract_test_blocks(file)
    if (length(content)==0){
        doc_message(._("  + No testing blocks found in `%s`.", file)) %if% verbose
        return(invisible(character(0)))
    }
    context.line <- sprintf("context('tests extracted from file `%s`')"
                           , if (full.path) file else basename(file))
    cat( .tests.head.lines, context.line, content, file=file.out, sep='\n', append=FALSE )

    #! testing blocks can be placed inside the same files as the source
    #! for the functions.  Wrap the lines in curly braces and place
    #! `if(FALSE)` before the opening brace, to denote that the code
    #! should not be run when sourced, such as when building a package.
    #! The `FALSE` may be abbreviated as `F`, but those are the only
    #! two acceptable options.  Also required is a documentation comment with a
    #! tag denoting that the block is for testing,
    #! either `@@testthat`, `@@testing`, or simply `@@test` are acceptable.
    #! The comment must be a documentation comment, regular comments are
    #! ignored, and the taging comment must be the first element in the block.
    #!
    return(s(attr(content, 'test.names'), test.file=file.out))
}
if(FALSE){#@testing extract_tests_to_file_ Basic
{'hello_world <- function(){
    print("hello world")
}
if(FALSE){#!@testthat
    expect_output(hello_world(), "hello world")
}

f2 <- function(){stop("this does nothing")}
if(F){#! @test
    expect_error(f2())
}
if(F){#! example
    hw()
}
'}-> text
tmp.in  <- normalizePath(tempfile("src-" , fileext=".R"), '/', FALSE)
tmp.out <- normalizePath(tempfile("test-", fileext=".R"), '/', FALSE)
if (!dir.exists(. <- dirname(tmp.in))) dir.create(.)

writeLines(text, tmp.in)

x <- extract_tests_to_file_(tmp.in, tmp.out, verbose=FALSE)

expect_true ( file.exists(tmp.out))
expect_equal( lines <- readLines(tmp.out)
            , c( "#! This file was automatically produced by the documentation package."
               , "#! Changes will be overwritten."
               , ""
               , sprintf("context('tests extracted from file `%s`')", basename(tmp.in))
               , sprintf("#line 4 \"%s\"", tmp.in)
               , "test_that('hello_world', {#!@testthat"
               , "    expect_output(hello_world(), \"hello world\")"
               , "})"
               , sprintf("#line 9 \"%s\"", tmp.in)
               , "test_that('f2', {#! @test"
               , "    expect_error(f2())"
               , "})"
               ))
expect_equal(x, s(c("hello_world", "f2"), test.file = tmp.out))
unlink(tmp.out)

expect_message( x <- extract_tests_to_file_(tmp.in, tmp.out, verbose=TRUE)
              , "* Extracting tests from file `.*`."
              )
expect_true (file.exists(tmp.out))
expect_equal( lines
            , c( "#! This file was automatically produced by the documentation package."
               , "#! Changes will be overwritten."
               , ""
               , sprintf("context('tests extracted from file `%s`')", basename(tmp.in))
               , sprintf("#line 4 \"%s\"", tmp.in)
               , "test_that('hello_world', {#!@testthat"
               , "    expect_output(hello_world(), \"hello world\")"
               , "})"
               , sprintf("#line 9 \"%s\"", tmp.in)
               , "test_that('f2', {#! @test"
               , "    expect_error(f2())"
               , "})"
               ))
expect_equal(x, s(c("hello_world", "f2"), test.file = tmp.out))

val <- extract_tests_to_file_(tmp.in, tmp.out, verbose=FALSE, force=FALSE)
expect_identical(val, s(character(0), test.file=tmp.out))
expect_message( val <- extract_tests_to_file_(tmp.in, tmp.out, verbose=TRUE, force=FALSE)
              , "  \\+ file `" %<<<% tmp.out %<<<% "` is newer\\.  SKIPPING\\."
              )

val <- extract_tests_to_file_(tmp.in, tmp.out, verbose=FALSE, force=TRUE)
expect_equal(val, s(c("hello_world", "f2"), test.file = tmp.out))

unlink(tmp.out)


withr::with_dir(dirname(tmp.in), {
    if (dir.exists('tests')) unlink('tests', recursive = TRUE, force = TRUE)
    x <- extract_tests_to_file_( file = basename(tmp.in)
                               , file.out = NULL
                               , NULL
                               , verbose=FALSE)
    file.out <- file.path('.', "test-" %<<<% basename(tmp.in), fsep='/')
    expect_true(file.exists(file.out))
    expect_equal( lines <- readLines(file.out)
                , c( "#! This file was automatically produced by the documentation package."
                   , "#! Changes will be overwritten."
                   , ""
                   , sprintf("context('tests extracted from file `%s`')", basename(tmp.in))
                   , sprintf("#line 4 \"%s\"", basename(tmp.in))
                   , "test_that('hello_world', {#!@testthat"
                   , "    expect_output(hello_world(), \"hello world\")"
                   , "})"
                   , sprintf("#line 9 \"%s\"", basename(tmp.in))
                   , "test_that('f2', {#! @test"
                   , "    expect_error(f2())"
                   , "})"
                   ))
    expect_equal(x, s(c("hello_world", "f2")
                     , test.file = file.out))
    unlink(file.out)
})
withr::with_dir(dirname(tmp.in), {
    if (!dir.exists('tests')) dir.create('tests') else
    if ( dir.exists('tests/testthat')) unlink('tests/testthat', TRUE, TRUE)
    expect_message({
        x <- extract_tests_to_file_( file = basename(tmp.in)
                                   , file.out = NULL
                                   , NULL
                                   , verbose=TRUE)
    }, "  \\+ `test.dir` not provided. Setting to `.*`")
    file.out <- "./tests/test-" %<<<% basename(tmp.in)
    expect_true(file.exists(file.out))
    expect_equal( lines <- readLines(file.out)
                , c( "#! This file was automatically produced by the documentation package."
                   , "#! Changes will be overwritten."
                   , ""
                   , sprintf("context('tests extracted from file `%s`')", basename(tmp.in))
                   , sprintf("#line 4 \"%s\"", basename(tmp.in))
                   , "test_that('hello_world', {#!@testthat"
                   , "    expect_output(hello_world(), \"hello world\")"
                   , "})"
                   , sprintf("#line 9 \"%s\"", basename(tmp.in))
                   , "test_that('f2', {#! @test"
                   , "    expect_error(f2())"
                   , "})"
                   ))
    expect_equal(x, s(c("hello_world", "f2"), test.file = file.out))
    unlink(file.out)
    unlink("tests", force = TRUE)
})
withr::with_dir(dirname(tmp.in), {
    if (!dir.exists('tests/testthat'))
        dir.create('tests/testthat', recursive = TRUE)
    tryCatch({
    expect_true(file.exists(basename(tmp.in)))
    expect_message({
        x <- extract_tests_to_file_( file = basename(tmp.in)
                                   , file.out = NULL
                                   , test.dir = NULL
                                   , verbose=TRUE)
    }, "  \\+ Writting extracted tests to `.*`.")
    file.out <- "./tests/testthat/test-" %<<<% basename(tmp.in)
    expect_true(file.exists(file.out))
    expect_equal( lines <- readLines(file.out)
                , c( "#! This file was automatically produced by the documentation package."
                   , "#! Changes will be overwritten."
                   , ""
                   , sprintf("context('tests extracted from file `%s`')", basename(tmp.in))
                   , sprintf("#line 4 \"%s\"", basename(tmp.in))
                   , "test_that('hello_world', {#!@testthat"
                   , "    expect_output(hello_world(), \"hello world\")"
                   , "})"
                   , sprintf("#line 9 \"%s\"", basename(tmp.in))
                   , "test_that('f2', {#! @test"
                   , "    expect_error(f2())"
                   , "})"
                   ))
    expect_equal(x, s(c("hello_world", "f2"), test.file = file.out))
    }, finally = unlink(file.path(tempdir(), "tests"), TRUE, TRUE))
})
expect_false(dir.exists(file.path(tempdir(), "tests")))

unlink(tmp.in)
}
if(FALSE){#@testing extract_tests_to_file_ setClass
{'
setClass("Test-Class")
if(FALSE){#!@test
    expect_true(TRUE)
    expect_is(getClass("Test-Class"), "classRepresentation")
}
'}-> class.text

tmp.in  <- tempfile("src-" , fileext=".R")
tmp.out <- tempfile("test-", fileext=".R")

writeLines(class.text, tmp.in)
x <- extract_tests_to_file_(tmp.in, tmp.out, verbose=FALSE)
lines <- readLines(tmp.out)

expect_true (file.exists(tmp.out))
expect_equal( lines
            , c( "#! This file was automatically produced by the documentation package."
               , "#! Changes will be overwritten."
               , ""
               , sprintf("context('tests extracted from file `%s`')", basename(tmp.in))
               , sprintf("#line 3 \"%s\"", tmp.in)
               , "test_that('setClass(\"Test-Class\", ...)', {#!@test"
               , "    expect_true(TRUE)"
               , "    expect_is(getClass(\"Test-Class\"), \"classRepresentation\")"
               , "})"
               )
            )
expect_equal(x, s("setClass(\"Test-Class\", ...)", test.file = tmp.out))

unlink(tmp.in)
unlink(tmp.out)
}
if(FALSE){#@testing extract_tests_to_file_ setMethod
'
setMethod("show", "Test-Class", function(x){cat("hi")})
if(FALSE){#!@test
    expect_true(TRUE)
}
'-> method.text
tmp.in  <- tempfile("src-" , fileext=".R")
tmp.out <- tempfile("test-", fileext=".R")

writeLines(method.text, tmp.in)
x <- extract_tests_to_file_(tmp.in, tmp.out, verbose=FALSE)
lines <- readLines(tmp.out)

expect_true (file.exists(tmp.out))
expect_equal( lines
            , c( "#! This file was automatically produced by the documentation package."
               , "#! Changes will be overwritten."
               , ""
               , sprintf("context('tests extracted from file `%s`')", basename(tmp.in))
               , sprintf("#line 3 \"%s\"", tmp.in)
               , "test_that('show,Test-Class-method', {#!@test"
               , "    expect_true(TRUE)"
               , "})"
               )
            )
expect_equal(x, s("show,Test-Class-method", test.file = tmp.out))

unlink(tmp.in)
unlink(tmp.out)
}
if(FALSE){#@testing extract_tests_to_file_ setGeneric
'
setGeneric("yolo", yolo::yolo)
if(FALSE){#!@test
    expect_true(TRUE)
}
'-> generic.text
tmp.in  <- tempfile("src-" , fileext=".R")
tmp.out <- tempfile("test-", fileext=".R")

writeLines(generic.text, tmp.in)
x <- extract_tests_to_file_(tmp.in, tmp.out, verbose=FALSE)
lines <- readLines(tmp.out)

expect_true (file.exists(tmp.out))
expect_equal( lines
            , c( "#! This file was automatically produced by the documentation package."
               , "#! Changes will be overwritten."
               , ""
               , sprintf("context('tests extracted from file `%s`')", basename(tmp.in))
               , sprintf("#line 3 \"%s\"", tmp.in)
               , "test_that('setGeneric(\"yolo\", ...)', {#!@test"
               , "    expect_true(TRUE)"
               , "})"
               )
            )
expect_equal(x, s("setGeneric(\"yolo\", ...)", test.file = tmp.out))
}
if(FALSE){#@testing extract_tests_to_file_ no test blocks
'hello_world <- function(){
    print("hello world")
}

f2 <- function(){stop("this does nothing")}
if(F){#! example
    hw()
}
'-> text
tmp.in  <- tempfile("src-" , fileext=".R")
tmp.out <- tempfile("test-", fileext=".R")

writeLines(text, tmp.in)
expect_message( x <- extract_tests_to_file_(tmp.in, tmp.out, verbose=TRUE)
              , class = "documentation-message")
expect_identical(x, character())
expect_false (file.exists(tmp.out))
}


#' @export
extract_tests <-
function( pkg = '.'     #< package to extract tests for.
        , filter = NULL
        , verbose  = default(verbose, FALSE) #< print messages
        , full.path = default(full.path, NA)
        , force = FALSE #< Force extraction of tests.
        ){
    #! Extract tests for testing directory.
    if (requireNamespace('devtools')){
        pkg <- devtools::as.package(pkg)
    } else if (is.character(pkg) && file.exists(file.path(pkg, "DESCRIPTION"))) { # nocov start
        desc <- read.dcf(file.path(pkg, "DESCRIPTION"))
        desc <- structure(as.list(desc), names=tolower(colnames(desc)))
        desc$path <- normalizePath(pkg)
        pkg <- structure(desc, class = 'package')
    }# nocov end
    if (.Platform$OS.type == "windows")
        pkg$path <- normalizePath(pkg$path, '/')  # nocov
    for(e in intersect(c('imports', 'suggests', 'depends', 'collate'), names(pkg)))
        pkg[[e]] <- trimws(strsplit(pkg[[e]], "\\s*,\\s*")[[1]], 'both')

    if (!"testthat" %in% pkg$suggests)
        doc_warning("testthat not found in suggests." %<<%
                    "`extract_tests` assumes a testthat infrastructure.")
    test.dir <- file.path(pkg$path, "tests", "testthat")
    if (!file.exists(test.dir)) {
        doc_message("Creating directory `"%<<<% test.dir %<<<%"`") %if% (verbose) #nocov
        dir.create(test.dir, recursive=TRUE)
    }
    if (!file.exists(.f <- file.path(test.dir, "..", "testthat.R"))){
        doc_message("creating file `"%<<<%normalizePath(.f)%<<<%"`") %if% (verbose)  #nocov
        cat( c( paste0("# This file was created by `documentation::extract_tests` on ", Sys.time(), ".")
              , "# Once present, this file will not be overwritten and changes will persist."
              , "# To recreate the default version of this file delete and rerun `extract_tests`."
              , 'library(testthat)'
              , sprintf('test_check("%s")', pkg$package)
              )
           , file=.f, sep='\n')
    }

    files <- if(is.na(full.path)){
        old <- setwd(dir=pkg$path)
        on.exit(setwd(old))
        file.path("R",
            list.files( "R", pattern="\\.r$", ignore.case=TRUE, full.names=FALSE))
    } else if (full.path) {
        list.files( file.path(pkg$path, "R"), pattern="\\.r$", ignore.case=TRUE, full.names=TRUE)
    } else {
        old <- setwd(dir=file.path(pkg$path, 'R'))
        on.exit(setwd(old))
        list.files( ".", pattern="\\.r$", ignore.case=TRUE, full.names=FALSE)
    }
    if (!is.null(filter)) {
        assert_that(is.string(filter))
        which.files <- grepl(filter, sub("\\.[rR]$", "", basename(files)), perl = TRUE)
        if (!any(which.files))
            doc_error("Filtered to no files to extract from.")
        files <- files[which.files]
    }
    structure( lapply( files, extract_tests_to_file_
                     , test.dir=test.dir
                     , verbose=verbose
                     , full.path = isTRUE(full.path)
                     , force = force
                     )
             , names = files)

}
if(FALSE){#@testing
    tmp.dir <- normalizePath(tempdir(), '/', TRUE)
    if (!dir.exists(tmp.dir)) dir.create(tmp.dir)
    package.skeleton("testExtractionTest", path=tmp.dir
                    , code_files = list.files(system.file("testExtractionTest", "R", package='documentation'), full=TRUE)
                    )
    pkg <- file.path(tmp.dir, "testExtractionTest", fsep='/')
    expect_warning( result <- extract_tests(pkg)
                  , "testthat not found in suggests. `extract_tests` assumes a testthat infrastructure.")
    test.dir <- file.path(tmp.dir, 'testExtractionTest', 'tests', 'testthat', fsep='/')
    expected <- structure( list( s(c( "setClass(\"Test-Class\", ...)"
                                    , "show,Test-Class-method"
                                    , "setGeneric(\"yolo\", ...)"
                                    )
                                  , test.file = file.path(test.dir, 'test-Class.R', fsep='/')
                                  )
                               , s("hello_world"
                                  , test.file = file.path(test.dir, 'test-function.R', fsep='/')
                                  )
                               )
                         , names = file.path('R', c('Class.R', 'function.R'))
                         )

    test.dir <- normalizePath(file.path(pkg, "tests", "testthat"), '/', FALSE)
    expect_identical(list.files(test.dir), c('test-Class.R', 'test-function.R'))

    file <- file.path(test.dir, 'test-Class.R')
    expect_identical( readLines(file)[c(1:5)]
                    , c( "#! This file was automatically produced by the documentation package."
                       , "#! Changes will be overwritten."
                       , ""
                       , "context('tests extracted from file `Class.R`')"
                       , "#line 4 \"R/Class.R\""
                       )
                    )

    expect_equal( result, expected)
    expect_true(dir.exists(file.path(pkg, "tests", "testthat")))
    expect_true(file.exists(file.path(pkg, "tests", "testthat", "test-Class.R")))
    expect_true(file.exists(file.path(pkg, "tests", "testthat", "test-function.R")))

    unlink(sapply(expected, attr, 'test.file'))
    expect_warning( result <- extract_tests(pkg, full.path = TRUE)
                  , "testthat not found in suggests. `extract_tests` assumes a testthat infrastructure.")
    expected <- s( expected
                 , names = file.path(pkg, 'R', c('Class.R', 'function.R'), fsep ="/"))
    expect_identical(result, expected)

    file <- file.path(test.dir, 'test-Class.R')
    from <- normalizePath(file.path(pkg, "R", "Class.R"), '/', FALSE)
    expect_identical( readLines(file)[c(1:5)]
                    , c( "#! This file was automatically produced by the documentation package."
                       , "#! Changes will be overwritten."
                       , ""
                       , "context('tests extracted from file `" %<<<% from %<<<% "`')"
                       , "#line 4 \"" %<<<% from %<<<%"\""
                       )
                    )


    unlink(sapply(expected, attr, 'test.file'))
    expect_warning( result <- extract_tests(pkg, full.path = FALSE)
                  , "testthat not found in suggests. `extract_tests` assumes a testthat infrastructure.")
    expected <- structure( expected
                         , names = c('Class.R', 'function.R')
                         )
    expect_identical(result, expected)

    file <- file.path(test.dir, 'test-Class.R')
    from <- normalizePath(file.path(pkg, "R", "Class.R"), '/', FALSE)
    expect_identical( readLines(file)[c(1:5)]
                    , c( "#! This file was automatically produced by the documentation package."
                       , "#! Changes will be overwritten."
                       , ""
                       , "context('tests extracted from file `" %<<<% basename(from) %<<<% "`')"
                       , "#line 4 \"" %<<<% basename(from) %<<<%"\""
                       )
                    )

    unlink(pkg, recursive=TRUE)
}
if(FALSE){#@testing
    package.skeleton("testExtractionTest", path=tempdir()
                    , code_files = list.files(system.file("testExtractionTest", "R", package='documentation'), full=TRUE)
                    )
    pkg <- file.path(tempdir(), "testExtractionTest")
    test.dir <- normalizePath(file.path(pkg, "tests", "testthat"), '/', mustWork = FALSE)

    expect_identical(list.files(test.dir, full.names = TRUE),character())
    expect_warning( result <- extract_tests(pkg, filter='Class', full.path = FALSE))

    expect_identical( result
                    , list("Class.R" = s(c( 'setClass("Test-Class", ...)'
                                          , 'show,Test-Class-method'
                                          , 'setGeneric("yolo", ...)'
                                          )
                                        , test.file = file.path(test.dir, 'test-Class.R', fsep='/')
                                        )
                           ))

    expect_true(dir.exists(test.dir))
    expect_identical( list.files(test.dir, full.names = FALSE)
                    , 'test-Class.R'
                    )
    unlink(pkg, recursive = TRUE)
}


# nocov start
test <- function(...){
    if (...length() == 0) {
        filter <- NULL
        pkg <- '.'
    } else
    if (!is.null(names(l <- list(...)))) {
        filter <- l$filter
        pkg <- l$pkg %||% '.'
    } else
    if (...length() == 1) {
        filter <- ..1
        pkg <- '.'
    } else
    if (...length() == 2) {
        filter <- ..2
        pkg <- ..1
    } else doc_error("too many arguments")
    tests <- extract_tests(pkg, filter=filter)
    doc_message(length(unlist(tests))%<<<%' test blocks extracted.\n')
    if (requireNamespace('devtools'))
        devtools::test(pkg=pkg, filter=filter, perl=TRUE)
    else
        stop('devtools is required to run the tests.')
}

addin_test <- function(){
    stopifnot(requireNamespace("rstudioapi"))
    project <- rstudioapi::getActiveProject()
    if (is.null(project)) project <- getwd()
    test(pkg=project, filter=NULL)
}
# nocov end

