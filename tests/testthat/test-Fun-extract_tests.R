#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Fun-extract_tests.R`')
#line 82 "R/Fun-extract_tests.R"
test_that('extract_tests_to_file_ Basic', {#@testing extract_tests_to_file_ Basic
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
expect_equal(x, c("hello_world", "f2"))
unlink(tmp.out)

expect_message( x <- extract_tests_to_file_(tmp.in, tmp.out, verbose=TRUE)
              , "* Extracting tests from file `" %<<<% tmp.in %<<<% "`."
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
expect_equal(x, c("hello_world", "f2"))
unlink(tmp.out)


withr::with_dir(dirname(tmp.in), {
    x <- extract_tests_to_file_( file = basename(tmp.in)
                               , file.out = NULL
                               , NULL
                               , verbose=FALSE)
    file.out <- "test-" %<<<% basename(tmp.in)
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
    expect_equal(x, c("hello_world", "f2"))
    unlink(file.out)
})
withr::with_dir(dirname(tmp.in), {
    dir.create('tests')
    x <- extract_tests_to_file_( file = basename(tmp.in)
                               , file.out = NULL
                               , NULL
                               , verbose=FALSE)
    file.out <- "tests/test-" %<<<% basename(tmp.in)
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
    expect_equal(x, c("hello_world", "f2"))
    unlink(file.out)
    unlink("tests", force = TRUE)
})
withr::with_dir(dirname(tmp.in), {
    dir.create('tests/testthat', recursive = TRUE)
    x <- extract_tests_to_file_( file = basename(tmp.in)
                               , file.out = NULL
                               , NULL
                               , verbose=FALSE)
    file.out <- "tests/testthat/test-" %<<<% basename(tmp.in)
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
    expect_equal(x, c("hello_world", "f2"))
    unlink(file.path(tempdir(), "tests"), TRUE, TRUE)
})
expect_false(dir.exists(file.path(tempdir(), "tests")))

unlink(tmp.in)
})
#line 225 "R/Fun-extract_tests.R"
test_that('extract_tests_to_file_ setClass', {#@testing extract_tests_to_file_ setClass
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
expect_equal(x, "setClass(\"Test-Class\", ...)")

unlink(tmp.in)
unlink(tmp.out)
})
#line 259 "R/Fun-extract_tests.R"
test_that('extract_tests_to_file_ setMethod', {#@testing extract_tests_to_file_ setMethod
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
expect_equal(x, "show,Test-Class-method")

unlink(tmp.in)
unlink(tmp.out)
})
#line 290 "R/Fun-extract_tests.R"
test_that('extract_tests_to_file_ setGeneric', {#@testing extract_tests_to_file_ setGeneric
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
expect_equal(x, "setGeneric(\"yolo\", ...)")
})
#line 379 "R/Fun-extract_tests.R"
test_that('#', {#@TESTING
    tmp.dir <- tempdir()
    if (!dir.exists(tmp.dir)) dir.create(tmp.dir)
    package.skeleton("testExtractionTest", path=tmp.dir
                    , code_files = list.files(system.file("testExtractionTest", "R", package='documentation'), full=TRUE)
                    )
    pkg <- file.path(tmp.dir, "testExtractionTest")
    expect_warning(result <- extract_tests(pkg), "testthat not found in suggests. `extract_tests` assumes a testthat infrastructure.")
    expected <- structure( list( c( "setClass(\"Test-Class\", ...)"
                                  , "show,Test-Class-method"
                                  , "setGeneric(\"yolo\", ...)"
                                  )
                               , "hello_world"
                               )
                         , names = file.path('R', c('Class.R', 'function.R'))
                         )

    test.dir <- normalizePath(file.path(pkg, "tests", "testthat"), '/')
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

    expect_warning( result <- extract_tests(pkg, full.path = TRUE)
                  , "testthat not found in suggests. `extract_tests` assumes a testthat infrastructure.")
    expected <- structure( list( c( "setClass(\"Test-Class\", ...)"
                                  , "show,Test-Class-method"
                                  , "setGeneric(\"yolo\", ...)"
                                  )
                               , "hello_world")
                         , names = normalizePath(file.path(pkg, 'R', c('Class.R', 'function.R')), "/")
                         )
    expect_identical(result, expected)

    file <- file.path(test.dir, 'test-Class.R')
    from <- normalizePath(file.path(pkg, "R", "Class.R"), '/')
    expect_identical( readLines(file)[c(1:5)]
                    , c( "#! This file was automatically produced by the documentation package."
                       , "#! Changes will be overwritten."
                       , ""
                       , "context('tests extracted from file `" %<<<% from %<<<% "`')"
                       , "#line 4 \"" %<<<% from %<<<%"\""
                       )
                    )


    expect_warning( result <- extract_tests(pkg, full.path = FALSE)
                  , "testthat not found in suggests. `extract_tests` assumes a testthat infrastructure.")
    expected <- structure( list( c( "setClass(\"Test-Class\", ...)"
                                  , "show,Test-Class-method"
                                  , "setGeneric(\"yolo\", ...)"
                                  )
                               , "hello_world")
                         , names = c('Class.R', 'function.R')
                         )
    expect_identical(result, expected)

    file <- file.path(test.dir, 'test-Class.R')
    from <- normalizePath(file.path(pkg, "R", "Class.R"), '/')
    expect_identical( readLines(file)[c(1:5)]
                    , c( "#! This file was automatically produced by the documentation package."
                       , "#! Changes will be overwritten."
                       , ""
                       , "context('tests extracted from file `" %<<<% basename(from) %<<<% "`')"
                       , "#line 4 \"" %<<<% basename(from) %<<<%"\""
                       )
                    )

    unlink(tmp.dir, recursive=TRUE)
})
