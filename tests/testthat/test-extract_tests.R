#! This file was automatically produced by documentation::extract_tests on  2018-08-03 22:20:01
#! changes will be overwritten.
context('tests extracted from file `extract_tests.R`')
#line 253 "/rdtf/documentation/R/extract_tests.R"
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
                         , names = file.path(pkg, 'R', c('Class.R', 'function.R')) )

    expect_equal( result, expected)
    expect_true(dir.exists(file.path(pkg, "tests", "testthat")))
    expect_true(file.exists(file.path(pkg, "tests", "testthat", "test-Class.R")))
    expect_true(file.exists(file.path(pkg, "tests", "testthat", "test-function.R")))

    unlink(tmp.dir, recursive=TRUE)
})
