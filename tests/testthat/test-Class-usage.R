#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Class-usage.R`')
#line 9 "R/Class-usage.R"
test_that('usage', {#@testing
    u <- usage()
    expect_is(u, 'usage')
    expect_is(u, 'Virtual/Usage')
    expect_is(u, 'expression')
})
#line 15 "R/Class-usage.R"
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
#line 63 "R/Class-usage.R"
test_that('as(`function`, "usage")', {#@testing
    val <- as(rnorm, 'usage')
    exp <- new('usage', expression(rnorm(n, mean=0, sd=1)))
    expect_identical(val, exp)

    fun <- dnorm
    attr(fun, 'name') <- 'dnorm'
    val2 <- as(fun, 'usage')
    exp2 <- new('usage', expression(dnorm(x, mean=0, sd=1, log=FALSE)))
    expect_identical(val2, exp2)

    v3 <- c(val, val2)

    attr(v3[[2]], 'additional') <- TRUE


    v3

})
#line 93 "R/Class-usage.R"
test_that('usage_waiver', {#@testing
    uv <- usage_waiver()
    expect_error(as.character(uv))
    expect_is(uv, "waiver")
    expect_is(uv, "Virtual/Usage")
    expect_error(usage_waiver(expression(1L)))
})
#line 100 "R/Class-usage.R"
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
#line 139 "R/Class-usage.R"
test_that('initialize,usage/S3method-method', {#@testing
    ex <- expression(html_to_Rd.em(html, ...))
    .Object <- new('usage/S3method')
    obj <- new('usage/S3method', ex, generic = 'html_to_Rd', signature = 'em')
    expect_is(obj, 'usage/S3method')
    expect_identical(S3Part(obj,TRUE), ex)
    expect_identical(obj@generic, as.name('html_to_Rd'))
    expect_identical(obj@signature, as.name('em'))
})
#line 168 "R/Class-usage.R"
test_that('as(`function`, "usage/S3method")', {#@testing
    val <- as(html_to_Rd.a, 'usage/S3method')
    exp <- s3usage( expression(html_to_Rd.a(html, ...))
                  , generic= 'html_to_Rd'
                  , signature = 'a'
                  )
    expect_identical(val, exp)
})
#line 237 "R/Class-usage.R"
test_that('`c.Virtual/Usage`', {#@testing
    U <- as(html_to_Rd, 'usage')
    V <- as(html_to_Rd.a, 'usage/S3method')
    val <- c(U,V)

    expect_identical(val[[1]], U)
    expect_identical(val[[2]], V)
    expect_identical(S3Part(val, TRUE), list(U,V))

    W <- as(html_to_Rd.abbr, 'usage/S3method')
    val2 <- c(val, list(W))
    expect_length(val2, 3L)
    expect_identical(val2[[1]], U)
    expect_identical(val2[[2]], V)
    expect_identical(val2[[3]], W)
})
