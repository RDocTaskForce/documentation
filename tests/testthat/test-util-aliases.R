#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `util-aliases.R`')
#line 26 "/rdtf/documentation/R/util-aliases.R"
test_that('s', {#@testing
    msg <- "An failure message"
    val <-s(FALSE, msg)
    expect_identical(attributes(val), list(msg=msg))


    val <- s(FALSE, msg, count = 5)
    expect_identical(attributes(val), list(msg=msg, count=5))

    val <- s(c(a=1, b=2), count=2)
    expect_identical(names(val), c('a','b'))
})
#line 44 "/rdtf/documentation/R/util-aliases.R"
test_that('cl', {#@testing
    x <- cl(TRUE, 'success')
    expect_is(x, 'success')

    y <- cl(x, 'a big success')
    expect_is(y, 'success')
    expect_is(y, 'a big success')

    expect_identical(cl('text', 'class')
                    , structure('text', class='class'))
})
#line 70 "/rdtf/documentation/R/util-aliases.R"
test_that('.T', {#@testing
    expect_equal(.T(._, s, cl, .T)
                , c('._', 's', 'cl', '.T')
                )
    expect_equal( .T(a=._, s, cl, .T)
                , c(a='._', s='s', cl='cl', .T='.T')
                )
})
#line 89 "/rdtf/documentation/R/util-aliases.R"
test_that('named', {#@testing
    a <- 1L
    b <- TRUE

    val <- named(a,b)
    expect_identical(val, list(a=a, b=b))

    val <- named(a,b,c='hello')
    expect_identical(val, list(a=a, b=b, c='hello'))
})
#line 105 "/rdtf/documentation/R/util-aliases.R"
test_that('get_attr', {#@testing
    expect_identical(get_attr(s(list(), test='hello'), 'test'), 'hello')
    expect_null     (get_attr(s(list(), test='hello'), 'test2'))
    expect_identical(get_attr(s(list(), test='hello'), 'test3', 'world'), 'world')
})
#line 117 "/rdtf/documentation/R/util-aliases.R"
test_that('fwd', {#@testing
    a <- s( list(Rd_symb("some"))
          , Rd_tag="\\keyword"
          , class=c("Rd_tag", 'Rd'))
    b <- forward_attributes(list, a)
    expect_identical(attributes(a), attributes(b))

    a <- s( matrix(1:6, 2, 3)
          , class = 'rectangle'
          , another='shape'
          )
    b <- forward_attributes(list(), a)
    expect_true('dim' %in% names(attributes(a)))
    expect_false('dim' %in% names(attributes(b)))
    expect_identical( attributes(a)[names(attributes(b))]
                    , attributes(b)
                    )
})
#line 146 "/rdtf/documentation/R/util-aliases.R"
test_that('regex_escape', {#@testing
    expect_identical(regex_escape('Vector(name)'), 'Vector\\(name\\)')
    expect_identical(regex_escape('my.function'), 'my\\.function')
})
#line 165 "/rdtf/documentation/R/util-aliases.R"
test_that('getAs', {#@testing
    f <- getAs( getClass('MethodDefinition')
              , getClass("usage/S4method")
              )
    expect_is(f, 'MethodDefinition')
})
