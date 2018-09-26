
#' @export
setClass("Virtual/Usage", contains='VIRTUAL')


#' @export
usage <- setClass("usage", contains=c("Virtual/Usage", "expression"))
if(FALSE){#@testing
    u <- usage()
    expect_is(u, 'usage')
    expect_is(u, 'Virtual/Usage')
    expect_is(u, 'expression')
}
if(FALSE){#@testing
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
}

usageClass <- getClass('usage', where = topenv(environment()))
setAs("call", 'usage', function(from)new(as.expression(from), Class=usageClass))
if(FALSE){#@usage
    call <- substitute(test(x,y))
    expect_is(call, 'call')
    expect_is(as(call, 'usage'), "usage")
    expect_identical(as(call, 'usage'), usage(expression(test(x,y))))
}
setAs("name", 'usage', function(from)new(as.expression(from), Class=usageClass))
if(FALSE){#@usage
    name <- substitute(hello)
    expect_is(name, 'name')
    expect_is(as(name, 'usage'), "usage")
    expect_identical(as(name, 'usage'), usage(expression(hello)))
}

#' @export
setClass("waiver")

#' @export
usage_waiver <-
setClass('usage-waiver', contains=c("Virtual/Usage", "waiver"))
if(FALSE){#@testing
    uv <- usage_waiver()
    expect_error(as.character(uv))
    expect_is(uv, "waiver")
    expect_is(uv, "Virtual/Usage")
    expect_error(usage_waiver(expression(1L)))
}
if(FALSE){#@testing
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
}

