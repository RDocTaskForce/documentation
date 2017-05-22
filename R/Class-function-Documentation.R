#' @include Class-arg-Documentation.R
#' @include Class-FormattedText.R
#' @include Fun-format_yaml.R

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
            .Object@name      <- as.name(name)
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
                 , value = "A function-Documentation object."
                 )
#~     trace('initialize', browser, signature='function-Documentation')
    object <- function_documentation()
    expect_equal(deparse(object@name), "<UNDEFINED>")
    
    object <- function_documentation(arguments = arg("only one arg"))
    
}

setMethod('format_yaml', 'function-Documentation', 
function( object
        , include = slotNames(object)
        , ...
        ){
    #' 
    
    i = include[[1L]]
    .hasSlot(object, i)
    
    lapply(include, slot, object=object)
    
#~     %>%
#~         Filter(length,.) %>% length
    
#~     as.yaml(slot(object, i))
})
if(F){
    include <- c('title', 'author')
    
    slot(object, include[[1]])
    slot(object, include[[2]])
    object@author
    
}


