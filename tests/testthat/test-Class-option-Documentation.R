#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Class-option-Documentation.R`')
#line 27 "R/Class-option-Documentation.R"
test_that('initialize,option-Documentation-method', {#! @testing
    docs <- option_documentation('anOption', 'a description')
    expect_identical(docs@key, 'anOption')
    expect_identical(docs@description, FormattedText('a description'))
    expect_identical(docs@constraints, list())
})
