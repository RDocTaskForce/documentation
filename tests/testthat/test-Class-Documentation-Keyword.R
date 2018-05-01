#! This file was automatically produced by documentation::extract_tests on  2018-04-30 17:06:19
#! changes will be overwritten.
context('tests extracted from file `C:/Users/aredd/Box Sync/Projects/rdtf/documentation/R/Class-Documentation-Keyword.R`')
#line 28 "C:/Users/aredd/Box Sync/Projects/rdtf/documentation/R/Class-Documentation-Keyword.R"
test_that('setClass("Documentation-Keyword", ...)', {#! @testing
    x <- new('Documentation-Keyword', 'utilities')
    expect_true(validObject(x))
    expect_error(new('Documentation-Keyword', 'utils'))
    expect_true(validObject(as('utilities', 'Documentation-Keyword')))
})
