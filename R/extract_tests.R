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


makeActiveBinding( '.tests.head.lines', function(){
    c( paste("#! This file was automatically produced by documentation::extract_tests on "
            , Sys.time()
            )
     , "#! changes will be overwritten."
     )
}, topenv())

#@internal
extract_tests_to_file_ <-
function( file              #< file to extract tests from
        , file.out  = NULL  #< file to write tests to, if provided must be fully specified, ie. `dir` will be ignored.
        , test.dir  = NULL  #< directory where to store extracted blocks.
        , verbose   = getOption('verbose', FALSE) #< Show progress messages?
        , full.path = FALSE
        ){
    if (verbose) message("* Extracting test from file `", file, "`.")
    if (is.null(file.out)){
        if (is.null(test.dir)){
            test.dir <- '.'
            if (file.exists(. <- file.path(test.dir, "tests"   ))) test.dir <- .
            if (file.exists(. <- file.path(test.dir, "testthat"))) test.dir <- .
            if (verbose) message("  + `test.dir` not provided. Setting to `", test.dir, "`")
        }
        file.out <- file.path(test.dir, sprintf("test-%s", basename(file)))
        if (verbose) message("  + Writting extracted tests to `", file.out,"`.")
    }

    #! Extract `if(F){#! @TESTTHAT }` blocks from file
    content <- parsetools::extract_test_blocks(file)
    if (length(content)==0){
        if(verbose) message("No testing blocks found in ", file)
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
    #! two acceptable options.  Also required is a documenation comment with a
    #! tag denoting that the block is for testing,
    #! either `@@testthat`, `@@testing`, or simply `@@test` are acceptable.
    #! The comment must be a documentation comment, regular comments are
    #! ignored, and the taging comment must be the first element in the block.
    #!
    return(attr(content, 'test.names'))
}
if(FALSE){#! @testings
{# Basic
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
tmp.in  <- tempfile("src-" , fileext=".R")
tmp.out <- tempfile("test-", fileext=".R")
if (!dir.exists(. <- dirname(tmp.in))) dir.create(.)

writeLines(text, tmp.in)

x <- extract_tests_to_file_(tmp.in, tmp.out, verbose=FALSE)
lines <- readLines(tmp.out)

expect_true (file.exists(tmp.out))
expect_equal( lines[-1]
            , c( "#! changes will be overwritten."
               , sprintf("context('tests extracted from file `%s`')", tmp.in)
               , sprintf("#line 4 \"%s\"", tmp.in)
               , "test_that('hello_world', {#!@testthat"
               , "    expect_output(hello_world(), \"hello world\")"
               , "})"
               , sprintf("#line 9 \"%s\"", tmp.in)
               , "test_that('f2', {#! @test"
               , "    expect_error(f2())"
               , "})"
               ))
expect_equal(x, c("hello_world", "f2"))
unlink(tmp.in)
unlink(tmp.out)
unlink(dirname(tmp.in))
}
{# setClass
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
expect_equal( lines[-(1:2)]
            , c( sprintf("context('tests extracted from file `%s`')", tmp.in)
               , sprintf("#line 3 \"%s\"", tmp.in)
               , "test_that(\"setClass('Test-Class', ...)\", {#!@test"                  
               , "    expect_true(TRUE)"                                                    
               , "    expect_is(getClass(\"Test-Class\"), \"classRepresentation\")"            
               , "})"
               )
            )
expect_equal(x, "setClass('Test-Class', ...)")

unlink(tmp.in)
unlink(tmp.out)
}
{# setMethod
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
expect_equal( lines[-(1:2)]
            , c( sprintf("context('tests extracted from file `%s`')", tmp.in)
               , sprintf("#line 3 \"%s\"", tmp.in)
               , "test_that('show.Test-Class', {#!@test"                  
               , "    expect_true(TRUE)"
               , "})"
               )
            )
expect_equal(x, "show.Test-Class")

unlink(tmp.in)
unlink(tmp.out)
}            
{# setGeneric
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
expect_equal( lines[-(1:2)]
            , c( sprintf("context('tests extracted from file `%s`')", tmp.in)
               , sprintf("#line 3 \"%s\"", tmp.in)
               , "test_that(\"setGeneric('yolo', ...)\", {#!@test"
               , "    expect_true(TRUE)"
               , "})"
               )
            )
expect_equal(x, "setGeneric('yolo', ...)")
}
}

#' @export
extract_tests <-
function( pkg = '.'     #< package to extract tests for.
        , verbose = getOption('verbose', FALSE) #< print messages
        ){
    #! Extract tests for testing directory.
    if (file.exists(file.path(pkg, "DESCRIPTION"))) {
        desc <- read.dcf(file.path(pkg, "DESCRIPTION"))
        desc <- structure(as.list(desc), names=tolower(colnames(desc)))
        desc$path <- normalizePath(pkg)
        pkg <- structure(desc, class = 'package')
    } else {
        requireNamespace('devtools')
        pkg <- devtools::as.package(pkg)
    }
    if (.Platform$OS.type == "windows") 
        pkg$path <- gsub("\\\\", "/", pkg$path)
    for(e in intersect(c('imports', 'suggests', 'depends', 'collate'), names(pkg)))
        pkg[[e]] <- trimws(strsplit(pkg[[e]], "\\s*,\\s*")[[1]], 'both')

    if (!"testthat" %in% pkg$suggests)
        warning("testthat not found in suggests. `extract_tests` assumes a testthat infrastructure.")
    test.dir <- file.path(pkg$path, "tests", "testthat")
    if (!file.exists(test.dir)) {
        if (verbose) message("create directory `", test.dir, "`")
        dir.create(test.dir, recursive=TRUE)
    }
    if (!file.exists(.f <- file.path(test.dir, "..", "testthat.R"))){
        if (verbose) message("createing file `", normalizePath(.f), "`")
        cat( c( paste0("# This file was created by `documentation::extract_tests` on ", Sys.time(), ".")
              , "# Once present, this file will not be overwritten and changes will persist."
              , "# To recreate the default version of this file delete and rerun `extract_tests`."
              , 'library(testthat)'
              , sprintf('test_check("%s")', pkg$package)
              )
           , file=.f, sep='\n')
    }
    files <- list.files( file.path(pkg, "R"), pattern="\\.r$", ignore.case=T, full.names=T)
    structure( lapply(files, extract_tests_to_file_, test.dir=test.dir, verbose=verbose)
             , names = files)
    
}
if(FALSE){#@TESTING
    tmp.dir <- tempdir()
    if (!dir.exists(tmp.dir)) dir.create(tmp.dir)
    package.skeleton("testExtractionTest", path=tmp.dir
                    , code_files = list.files(system.file("testExtractionTest", "R", package='documentation'), full=TRUE)
                    )
    pkg <- file.path(tmp.dir, "testExtractionTest")
    expect_warning(result <- extract_tests(pkg), "testthat not found in suggests. `extract_tests` assumes a testthat infrastructure.")
    expect_equal( result
                , structure( list( c( "setClass('Test-Class', ...)"
                                    , "show.Test-Class"
                                    , "setGeneric('yolo', ...)"
                                    ) 
                                 , "hello_world"
                                 ), names = file.path(pkg, 'R', c('Class.R', 'function.R')) )
                )
    expect_true(dir.exists(file.path(pkg, "tests", "testthat")))
    expect_true(file.exists(file.path(pkg, "tests", "testthat", "test-Class.R")))
    expect_true(file.exists(file.path(pkg, "tests", "testthat", "test-function.R")))

    unlink(tmp.dir, recursive=TRUE)
}

test <- function(pkg = ".", filter=NULL, extract=TRUE, ...){
    tests <- extract_tests()
    message(length(unlist(tests)), ' tests extracted.')
    if (requireNamespace('devtools'))
        devtools::test(pkg=pkg, filter=filter, ...)
    else
        stop('devtools is required to run the tests.')
}

addin_test <- function(){
    stopifnot(requireNamespace("rstudioapi"))
    project <- rstudioapi::getActiveProject()
    if(is.null(project))
        project <- getwd()
    test(project)
}


