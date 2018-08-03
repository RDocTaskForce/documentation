#! This file was automatically produced by documentation::extract_tests on  2018-08-03 22:20:02
#! changes will be overwritten.
context('tests extracted from file `util-undefined.R`')
#line 11 "/rdtf/documentation/R/util-undefined.R"
test_that('.undefined', {#@testing
    expect_true(.is_undefined(.undefined))
    expect_false(.is_undefined(as.name("my_name")))
})
