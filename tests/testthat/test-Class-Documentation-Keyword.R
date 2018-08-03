#! This file was automatically produced by documentation::extract_tests on  2018-08-03 01:24:28
#! changes will be overwritten.
context('tests extracted from file `Class-Documentation-Keyword.R`')
#line 28 "/rdtf/documentation/R/Class-Documentation-Keyword.R"
test_that('setClass("Documentation-Keyword", ...)', {#! @testing
    x <- new('Documentation-Keyword', 'utilities')
    expect_true(validObject(x))
    expect_error(new('Documentation-Keyword', 'utils'))
    expect_true(validObject(as('utilities', 'Documentation-Keyword')))
})
