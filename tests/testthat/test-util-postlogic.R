#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `util-postlogic.R`')
#line 34 "R/util-postlogic.R"
test_that('unless-then logic', {#@testing unless-then logic
    if (exists('x')) rm(list='x')
    val <- (x <- 'it is supposed to be evaluated') %unless% FALSE
    expect_equal(val, 'it is supposed to be evaluated')

    if (exists('x')) rm(list='x')
    val <- (x <- 'it still evaluated') %unless% TRUE
    expect_null(val)
    expect_false(exists('x'))

    if (exists('x')) rm(list='x')
    val <- (x <- 'it still evaluated') %unless% TRUE %then% "should get this"
    expect_equal(val, "should get this")
    expect_false(exists('x'))

    if (exists('x')) rm(list='x')
    val <- (x <- 'it is supposed to be evaluated') %unless% FALSE %then% "should not get this"
    expect_equal(val, 'it is supposed to be evaluated')
    expect_true(exists('x'))
    expect_equal(x, 'it is supposed to be evaluated')

    if (exists('x')) rm(list='x')
    expect_error( 'this' %if% 'wont' %then% 'work'
                , "Infix opperator '%then%' can only be used" %<<%
                  "following an '%unless%' infix."
                )
})
#line 100 "R/util-postlogic.R"
test_that('if-otherwise logic', {#@testing if-otherwise logic
    if (exists('x')) rm(list='x')
    val <- (x <- 'it still evaluated') %if% FALSE
    expect_null(val)
    expect_false(exists('x'))

    if (exists('x')) rm(list='x')
    val <- (x <- 'it still evaluated') %if% FALSE %otherwise% "should get this"
    expect_equal(val, "should get this")
    expect_false(exists('x'))

    if (exists('x')) rm(list='x')
    val <- (x <- 'it is supposed to be evaluated') %if% TRUE %otherwise% "should not get this"
    expect_equal(val, 'it is supposed to be evaluated')
    expect_true(exists('x'))
    expect_equal(x, 'it is supposed to be evaluated')

    if (exists('x')) rm(list='x')
    expect_error( 'this' %unless% 'wont' %otherwise% 'work'
                , "Infix opperator '%otherwise%' can only be used" %<<%
                  "following an '%if%' infix."
                )
})
