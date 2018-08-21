#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Class-usage.R`')
#line 6 "R/Class-usage.R"
test_that('usage', {#@testing
    u <- usage()
    expect_is(u, 'usage')
    expect_is(u, 'Virtual/Usage')
    expect_is(u, 'expression')
})
#line 12 "R/Class-usage.R"
test_that('usage', {#@testing
    FD <-
    setClass('FD-test'
            , slots = c( name       = 'name'
                       , usage      = 'Virtual/Usage'
                       , arguments  = 'ArgumentList'
                       )
            )
    object <- FD( name =as.name('test')
                , arguments=AL(arg_('x', 'first'), arg_('y', 'second'))
                , usage = usage(expression(test(x,y)))
                )
    expect_is(object@usage, 'usage')
    expect_is(object@usage, 'Virtual/Usage')
    expect_is(object@usage, 'expression')
    removeClass('FD-test')
})
#line 50 "R/Class-usage.R"
test_that('usage_waiver', {#@testing
    uv <- usage_waiver()
    expect_error(as.character(uv))
    expect_is(uv, "waiver")
    expect_is(uv, "Virtual/Usage")
    expect_error(usage_waiver(expression(1L)))
})
#line 57 "R/Class-usage.R"
test_that('usage_waiver', {#@testing
    FD <-
    setClass('FD-test'
            , slots = c( name       = 'name'
                       , usage      = 'Virtual/Usage'
                       , arguments  = 'ArgumentList'
                       )
            )
    object <- FD( name =as.name('test')
                , arguments=AL(arg_('x', 'first'), arg_('y', 'second'))
                , usage = usage_waiver()
                )
    expect_is(object@usage, 'waiver')
    expect_is(object@usage, 'Virtual/Usage')
    removeClass('FD-test')
})
