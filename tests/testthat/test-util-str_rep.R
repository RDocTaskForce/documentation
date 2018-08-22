#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `util-str_rep.R`')
#line 6 "R/util-str_rep.R"
test_that('str_rep', {#@testing
    expect_equal(str_rep('#', 3), '###')
    expect_equal(str_rep(c('r', 'l'), 5), "rlrlr")
})
#line 13 "R/util-str_rep.R"
test_that('space', {#@testing
    expect_identical(space(), ' ')
    expect_identical(space(0), '')
    expect_identical(space(3), '   ')
})
#line 22 "R/util-str_rep.R"
test_that('is_whitespace', {#@testing
    expect_true(is_whitespace(" "))
    expect_true(is_whitespace("\t"))
    expect_false(is_whitespace("t"))
    expect_false(is_whitespace(""))
})
