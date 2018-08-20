#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Fun-extract_tests.R`')
#line 271 "R/Fun-extract_tests.R"
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
