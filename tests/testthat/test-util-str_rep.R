#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `util-str_rep.R`')
#line 6 "/rdtf/documentation/R/util-str_rep.R"
test_that('str_rep', {#@testing
    expect_equal(str_rep('#', 3), '###')
    expect_equal(str_rep(c('r', 'l'), 5), "rlrlr")
})
