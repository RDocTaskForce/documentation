#! This file was automatically produced by documentation::extract_tests on  2018-08-03 22:20:01
#! changes will be overwritten.
context('tests extracted from file `Class-option-documentation.R`')
#line 23 "/rdtf/documentation/R/Class-option-documentation.R"
test_that('initialize,option-Documentation-method', {#! @testing
    docs <- option_documentation('anOption', 'a description')
    expect_identical(docs@key, 'anOption')
    expect_identical(docs@description, FormattedText('a description'))
    expect_identical(docs@constraints, list())
})
