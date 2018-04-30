#' @include Class-arg-Documentation.R
#' @include Class-FormattedText.R

#' @export function_documentation
#' @export
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
        if(!missing(name))
            .Object@name <- as.name(name)
        else
            .Object@name <- as.name("<UNDEFINED>")
        if(inherits(arguments, 'ArgumentList'))
            .Object@arguments <- arguments
        else if(  inherits(arguments, 'arg-Documentation'))
            .Object@arguments <- ArgumentList(arguments)
        else if(inherits(arguments, 'list'))
            .Object@arguments <- new('ArgumentList', arguments)
        else 
            .Object@arguments <- as(arguments, 'ArgumentList')
        .Object@value     <- as(value, 'FormattedText')
        if(missing(usage))
            usage <- as.call(c(.Object@name, sapply(arguments, slot, 'name')))
        .Object@usage     <- as.call(usage)
        .Object
    })
if(FALSE){#! @testing
    empty.object <- new( "function-Documentation")
    expect_is(empty.object, "function-Documentation")
    
    named.object <- new("function-Documentation", name = "Heisenburg")
    expect_is(named.object,"function-Documentation")
    expect_equal(deparse(getElement(named.object, 'name')), "Heisenburg")
    
    named.object <- new("function-Documentation", name = as.name("Heisenburg"))
    
    
    object <- new( "function-Documentation"
                 , name = as.name('function_documentation')
                 , title = 'Create function documentation'
                 , author = person('Andrew', 'Redd', email='andrew.redd@hsc.utah.edu')
                 , usage= call('function_documentation', as.name('name'), as.name('arguments'), as.name('usage'), as.name('...'))
                 , arguments = ArgumentList( arg(name     , "Name of the function")
                                           , arg(arguments, "Argument list"               , class="ArgumentList")
                                           , arg(value    , "Return value of the function")
                                           , arg(usage    , "Usage string to override default, constructed from the name and arguments.", class="call")
                                           , arg('...'    , "other arguments to contruct the Documentation object.")
                                           )
                 , description = "create documentation for a function"
                 , value = "A function-Documentation object."
                 )
    object <- function_documentation()
    expect_equal(deparse(object@name), "<UNDEFINED>")
}

setMethod('documentation<-', c('function', 'function-Documentation'), 
function(object, value){
    if (getOption("documentation::verbose", FALSE) && !is.null(attr(object, 'documentation')))
        message("documentation already exists, replacing documentation")
    #TODO 
    # this evaluates to `*tmp*` not to the name of the object.
    # How do we fix this?
    if (deparse(value@name) == "<UNDEFINED>")
        value@name <- substitute(object)
        
    attr(object, 'documentation') <- value
    object
})
if(FALSE){#@testing
    trace("documentation<-", signature = c('function', 'function-Documentation'), browser)
    hw <- function(){print("hello world")}
    documentation(hw) <- function_documentation(title = "the standard Hello world")


}
