#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Class-Documentation-Keyword.R`')
#line 29 "/rdtf/documentation/R/Class-Documentation-Keyword.R"
test_that('setClass("Documentation-Keyword", ...)', {#! @testing
    x <- new('Documentation-Keyword', 'utilities')
    expect_true(validObject(x))
    expect_error(new('Documentation-Keyword', 'utils'))
    expect_true(validObject(as('utilities', 'Documentation-Keyword')))
})
