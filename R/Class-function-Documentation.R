#' @include Class-arg-Documentation.R
#' @include Class-FormattedText.R

function_documentation <- 
setClass('function-Documentation', contains = 'Documentation'
        , slots = c( name       = 'name' 
                   , usage      = 'call'
                   , arguments  = 'ArgumentList'
                   , value      = 'FormattedText'
                   )
        )
setMethod('initialize', 'function-Documentation',
    function( .Object
            , name
            , arguments = ArgumentList()
            , value     = NA_character_
            , usage
            , ...
            ){
        .Object           <- callNextMethod(.Object, ...)
        .Object@name      <- as.name(name)
        .Object@arguments <- arguments
        .Object@value     <- as(value, 'FormattedText')
        if(missing(usage))
            usage <- as.call(c(.Object@name, sapply(arguments, slot, 'name')))
        .Object@usage     <- as.call(usage)
        .Object
    })

if(FALSE){#! @testing
    a <- new( "function-Documentation"
            , name = as.name('function_documentation')
            , usage= call('function_documentation', as.name('name'), as.name('arguments'), as.name('usage'), as.name('...'))
            , arguments = ArgumentList( arg(name     , "Name of the function")
                                      , arg(arguments, "Argument list"               , class="ArgumentList")
                                      , arg(value    , "Return value of the function")
                                      , arg(usage    , "Usage string to override default, constructed from the name and arguments.", class="call")
                                      , arg('...'    , "other arguments to contruct the Documentation object.")
                                      )
            , value = "A function-Documentation object."
            )
}

