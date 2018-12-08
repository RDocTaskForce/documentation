#' @include utils.R
#' @include Class-Vector.R

### Class: Virtual/Usage #####
#' @export
setClass("Virtual/Usage", contains='VIRTUAL')

### Class: usage #####
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

### call → usage #####
setAs("call", 'usage', function(from)new(as.expression(from), Class=usageClass))
if(FALSE){#@usage
    call <- substitute(test(x,y))
    expect_is(call, 'call')
    expect_is(as(call, 'usage'), "usage")
    expect_identical(as(call, 'usage'), usage(expression(test(x,y))))
}
### name → usage #####
setAs("name", 'usage', function(from)new(as.expression(from), Class=usageClass))
if(FALSE){#@usage
    name <- substitute(hello)
    expect_is(name, 'name')
    expect_is(as(name, 'usage'), "usage")
    expect_identical(as(name, 'usage'), usage(expression(hello)))
}

### function → usage #####
setAs("function", 'usage', function(from){
    name <- attr(from, 'name') %||% name_me(from) %||% match.call(call=sys.call(-1))$from
    args <- formals(from)

    if (is_registered_S3method(from))
        return(as(from, "usage/S3method"))

    for (i in which(nchar(args)==0)){
        args[[i]] <- as.name(names(args)[[i]])
        names(args)[[i]] <- ''
    }
    as(as.call(c(as.name(name), args)), 'usage')
})
if(FALSE){#@testing
    val <- as(rnorm, 'usage')
    exp <- new('usage', expression(rnorm(n, mean=0, sd=1)))
    expect_identical(val, exp)

    fun <- dnorm
    attr(fun, 'name') <- 'dnorm'
    val2 <- as(fun, 'usage')
    exp2 <- new('usage', expression(dnorm(x, mean=0, sd=1, log=FALSE)))
    expect_identical(val2, exp2)


    val <- as(solve.qr, 'usage')
    expect_is(val, 'usage')
    expect_is_exactly(val, 'usage/S3method')
}



### Class: waiver #####
#' @export
setClass("waiver")

### Class: usage_waiver #####
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


### Class: usage/S3method #####
#' @export
s3usage <-
setClass("usage/S3method", contains = "usage"
        , representation( generic = 'name'
                        , signature = 'name'
                        ))
### usage/S3method::initialize #####
#' @export
setMethod("initialize", "usage/S3method",
    function(.Object, ex=expression(), generic=NULL, signature=NULL){
        if (is.call(ex))
            ex <- as.expression(ex)
        if(length(ex))
            S3Part(.Object) <- ex
        if (!is.null(generic))
            .Object@generic <- as.name(generic)
        if (!is.null(signature))
            .Object@signature <- as.name(signature)
        return(.Object)
    })
if(FALSE){#@testing
    ex <- expression(html_to_Rd.em(html, ...))
    .Object <- new('usage/S3method')
    obj <- new('usage/S3method', ex, generic = 'html_to_Rd', signature = 'em')
    expect_is(obj, 'usage/S3method')
    expect_identical(S3Part(obj,TRUE), ex)
    expect_identical(obj@generic, as.name('html_to_Rd'))
    expect_identical(obj@signature, as.name('em'))
}

### function → usage/S3method #####
setAs("function", 'usage/S3method', function(from){
    name <- attr(from, 'name') %||% name_me(from) %||% match.call(call=sys.call(-1))$from
    generic <- attr(from, 'generic') %||%
               strsplit(as.character(name), '.', fixed=TRUE)[[1L]][[1L]]
    signature <- attr(from, 'signature') %||%
                gsub(generic %<<<% "\\.", '', as.character(name))

    args <- formals(from)

    for (i in which(nchar(args)==0)){
        args[[i]] <- as.name(names(args)[[i]])
        names(args)[[i]] <- ''
    }
    new('usage/S3method', as.call(c(as.name(name), args))
                        , generic = generic
                        , signature=signature
                        )
})
if(FALSE){#@testing
    val <- as(solve.qr, 'usage/S3method')
    exp <- s3usage( expression(solve.qr(a, b, ...))
                  , generic= 'solve'
                  , signature = 'qr'
                  )
    expect_identical(val, exp)
}


### Class: usage/S4method #####
#' @export
s4usage <-
setClass("usage/S4method", contains = "usage"
        , representation( generic = 'name'
                        , signature = 'signature'
                        ))
setMethod("initialize", "usage/S4method",
    function(.Object, ex=expression(), generic=NULL, signature=NULL){
        if (is.call(ex))
            ex <- as.expression(ex)
        if(length(ex))
            S3Part(.Object) <- ex
        if (!is.null(generic))
            .Object@generic <- as.name(generic)
        if (!is.null(signature))
            .Object@signature <- as(signature, 'signature')
        return(.Object)
    })
if(FALSE){#@testing
    val <- s4usage(substitute(.(ex=expression(), generic=NULL, signature=NULL))
                  , generic = 'initialize'
                  , signature = c(.Object='usage/S4method')
                  )
    expect_is(val, 'usage/S4method')
    expect_identical(val@generic, as.name('initialize'))
    expect_identical(val@signature, as(c(.Object='usage/S4method'), 'signature'))
}

### function → usage/S4method #####
setAs("MethodDefinition", 'usage/S4method', function(from){
    generic <- as.name(from@generic)
    signature <- from@defined

    args <- formals(from)

    for (i in which(nchar(args)==0)){
        args[[i]] <- as.name(names(args)[[i]])
        names(args)[[i]] <- ''
    }
    new('usage/S4method', as.call(c(as.name(generic), args))
                        , generic = generic
                        , signature=signature
                        )
})
setAs( "MethodDefinition", 'usage'
     , getAs('MethodDefinition', s4usage)
     )
if(FALSE){#@testing Coersion from MethodDefinition
    from <- getMethod('as.list', 'Documentation')
    val <- as(from, 'usage/S4method')
    exp <- s4usage( expression(as.list(x, ...))
                  , generic = 'as.list'
                  , signature = s(signature(x='Documentation'), package='documentation')
                  )
    expect_identical(val, exp)

    val2 <- as(from, 'usage')
    expect_identical(val, val2)
}

### Class: UsageList #####
#' @exportClass UsageList
#' @S3method [ UsageList
#' @S3method [<- UsageList
#' @S3method c Virtual/Usage
#' @S3method c UsageList
#' @S3method unique UsageList
UsageList <- setVector("Virtual/Usage", "UsageList", where=environment())
if(FALSE){#@testing c.Virtual/Usage
    U <- as(solve, 'usage')
    V <- as(solve.qr, 'usage/S3method')
    val <- c(U,V)

    expect_identical(val[[1]], U)
    expect_identical(val[[2]], V)
    expect_identical(S3Part(val, TRUE), list(U,V))

    W <- as(solve.default, 'usage/S3method')
    val2 <- c(val, W)
    expect_is(val2, 'UsageList')
    expect_length(val2, 3L)
    expect_identical(val2[[1]], U)
    expect_identical(val2[[2]], V)
    expect_identical(val2[[3]], W)
}
