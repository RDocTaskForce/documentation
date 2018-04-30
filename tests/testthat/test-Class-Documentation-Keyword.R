#! This file was automatically produced by documentation::extract_tests on  2017-06-20 13:15:39
#! changes will be overwritten.
context('tests extracted from file `/mnt/data/projects/rdtf/documentation/R/Class-Documentation-Keyword.R`')
#line 28 "/mnt/data/projects/rdtf/documentation/R/Class-Documentation-Keyword.R"
test_that("setClass('Documentation-Keyword', ...)", {#! @testing
    x <- new('Documentation-Keyword', 'utilities')
    expect_true(validObject(x))
    
    expect_error(new('Documentation-Keyword', 'utils'))
    
    validObject(as('utilities', 'Documentation-Keyword'))
    
    
    
})
