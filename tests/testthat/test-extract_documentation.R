#! This file was automatically produced by documentation::extract_tests on  2018-05-23 19:03:27
#! changes will be overwritten.
context('tests extracted from file `extract_documentation.R`')
#line 45 "/rdtf/documentation/R/extract_documentation.R"
test_that('extract_documentation.function', {#@testing
    test.file <- system.file("examples", "example_function1.R", package='documentation')
    source( test.file, keep.source=TRUE)
    expect_true(exists("example_function1"))
    pd <- get_parse_data(example_function1)

    object <- example_function1
        
    docs <- extract_documentation(example_function1)
    
})
