#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `util-undefined.R`')
#line 11 "R/util-undefined.R"
test_that('.undefined', {#@testing
    expect_true(.is_undefined(.undefined))
    expect_false(.is_undefined(as.name("my_name")))
})
