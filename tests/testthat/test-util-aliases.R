#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `util-aliases.R`')
#line 26 "R/util-aliases.R"
test_that('s', {#@testing
    msg <- "An failure message"
    val <-s(FALSE, msg)
    expect_identical(attributes(val), list(msg=msg))


    val <- s(FALSE, msg, count = 5)
    expect_identical(attributes(val), list(msg=msg, count=5))

    val <- s(c(a=1, b=2), count=2)
    expect_identical(names(val), c('a','b'))
})
#line 44 "R/util-aliases.R"
test_that('cl', {#@testing
    x <- cl(TRUE, 'success')
    expect_is(x, 'success')

    y <- cl(x, 'a big success')
    expect_is(y, 'success')
    expect_is(y, 'a big success')

    expect_identical(cl('text', 'class')
                    , structure('text', class='class'))
})
#line 70 "R/util-aliases.R"
test_that('.T', {#@testing
    expect_equal(.T(._, s, cl, .T)
                , c('._', 's', 'cl', '.T')
                )
    expect_equal( .T(a=._, s, cl, .T)
                , c(a='._', s='s', cl='cl', .T='.T')
                )
})
#line 86 "R/util-aliases.R"
test_that('get_attr', {#@testing
    expect_identical(get_attr(s(list(), test='hello'), 'test'), 'hello')
    expect_null     (get_attr(s(list(), test='hello'), 'test2'))
    expect_identical(get_attr(s(list(), test='hello'), 'test3', 'world'), 'world')
})
