#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Class-usage.R`')
#line 9 "/rdtf/documentation/R/Class-usage.R"
test_that('usage', {#@testing
    u <- usage()
    expect_is(u, 'usage')
    expect_is(u, 'Virtual/Usage')
    expect_is(u, 'expression')
})
#line 15 "/rdtf/documentation/R/Class-usage.R"
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
#line 66 "/rdtf/documentation/R/Class-usage.R"
test_that('as(`function`, "usage")', {#@testing
    val <- as(rnorm, 'usage')
    exp <- new('usage', expression(rnorm(n, mean=0, sd=1)))
    expect_identical(val, exp)

    fun <- dnorm
    attr(fun, 'name') <- 'dnorm'
    val2 <- as(fun, 'usage')
    exp2 <- new('usage', expression(dnorm(x, mean=0, sd=1, log=FALSE)))
    expect_identical(val2, exp2)


    val <- as(html_to_Rd.a, 'usage')
    expect_is(val, 'usage')
    expect_is_exactly(val, 'usage/S3method')
})
#line 93 "/rdtf/documentation/R/Class-usage.R"
test_that('usage_waiver', {#@testing
    uv <- usage_waiver()
    expect_error(as.character(uv))
    expect_is(uv, "waiver")
    expect_is(uv, "Virtual/Usage")
    expect_error(usage_waiver(expression(1L)))
})
#line 100 "/rdtf/documentation/R/Class-usage.R"
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
#line 139 "/rdtf/documentation/R/Class-usage.R"
test_that('initialize,usage/S3method-method', {#@testing
    ex <- expression(html_to_Rd.em(html, ...))
    .Object <- new('usage/S3method')
    obj <- new('usage/S3method', ex, generic = 'html_to_Rd', signature = 'em')
    expect_is(obj, 'usage/S3method')
    expect_identical(S3Part(obj,TRUE), ex)
    expect_identical(obj@generic, as.name('html_to_Rd'))
    expect_identical(obj@signature, as.name('em'))
})
#line 168 "/rdtf/documentation/R/Class-usage.R"
test_that('as(`function`, "usage/S3method")', {#@testing
    val <- as(html_to_Rd.a, 'usage/S3method')
    exp <- s3usage( expression(html_to_Rd.a(html, ...))
                  , generic= 'html_to_Rd'
                  , signature = 'a'
                  )
    expect_identical(val, exp)
})
#line 197 "/rdtf/documentation/R/Class-usage.R"
test_that('initialize,usage/S4method-method', {#@testing
    val <- s4usage(substitute(.(ex=expression(), generic=NULL, signature=NULL))
                  , generic = 'initialize'
                  , signature = c(.Object='usage/S4method')
                  )
    expect_is(val, 'usage/S4method')
    expect_identical(val@generic, as.name('initialize'))
    expect_identical(val@signature, as(c(.Object='usage/S4method'), 'signature'))
})
#line 226 "/rdtf/documentation/R/Class-usage.R"
test_that('Coersion from MethodDefinition', {#@testing Coersion from MethodDefinition
    from <- getMethod('as.list', 'Documentation')
    val <- as(from, 'usage/S4method')
    exp <- s4usage( expression(as.list(x, ...))
                  , generic = 'as.list'
                  , signature = s(signature(x='Documentation'), package='documentation')
                  )
    expect_identical(val, exp)

    val2 <- as(from, 'usage')
    expect_identical(val, val2)
})
#line 247 "/rdtf/documentation/R/Class-usage.R"
test_that('c.Virtual/Usage', {#@testing c.Virtual/Usage
    U <- as(html_to_Rd, 'usage')
    V <- as(html_to_Rd.a, 'usage/S3method')
    val <- c(U,V)

    expect_identical(val[[1]], U)
    expect_identical(val[[2]], V)
    expect_identical(S3Part(val, TRUE), list(U,V))

    W <- as(html_to_Rd.abbr, 'usage/S3method')
    val2 <- c(val, W)
    expect_is(val2, 'UsageList')
    expect_length(val2, 3L)
    expect_identical(val2[[1]], U)
    expect_identical(val2[[2]], V)
    expect_identical(val2[[3]], W)
})
