#' @include setup-set_old_classes.R
#' @include utils.R
#' @include Class-arg-Documentation.R
#' @include Class-FormattedText.R
#' @include Class-usage.R

### Class: function-Documentation #####
#' @export
function_documentation <-
setClass('function-Documentation', contains = 'BaseDocumentation'
        , slots = c( name       = 'name'
                   , usage      = 'Virtual/Usage'
                   , arguments  = 'ArgumentList'
                   , value      = 'FormattedText'
                   )
        )
functionDocumentation <- getClass('function-Documentation')
#' @export
setMethod('initialize', 'function-Documentation',
    function( .Object
            , name
            , arguments = ArgumentList()
            , value     = NA_character_
            , usage
            , ...
            ){
        if(!missing(name))
            .Object@name <- as.name(name)
        else
            .Object@name <- .undefined
        if(inherits(arguments, 'ArgumentList'))
            .Object@arguments <- arguments
        else if(  inherits(arguments, 'arg-Documentation'))
            .Object@arguments <- ArgumentList(arguments)
        else if(inherits(arguments, 'list'))
            .Object@arguments <- new('ArgumentList', arguments)
        else
            .Object@arguments <- as(arguments, 'ArgumentList')
        .Object@value <- FT(value)
        .Object@usage <- if (missing(usage))             usage_waiver()    else
                         if (is(usage, 'Virtual/Usage')) usage             else
                                                         as(usage, 'usage')
        .Object <- callNextMethod(.Object, ...)
        .Object
    })
if(FALSE){#! @testing
    empty.object <- new( "function-Documentation")
    expect_is(empty.object, "function-Documentation")

    named.object <- new("function-Documentation", name = "Heisenburg")
    expect_is(named.object,"function-Documentation")
    expect_equal(deparse(getElement(named.object, 'name')), "Heisenburg")

    fun.args <- ArgumentList( arg(name     , "Name of the function")
                            , arg(arguments, "Argument list"               , class="ArgumentList")
                            , arg(value    , "Return value of the function")
                            , arg(usage    , "Usage string to override default, constructed from the name and arguments.", class="call")
                            , arg('...'    , "other arguments to contruct the Documentation object.")
                            )
    object <- new( "function-Documentation"
                 , name = as.name('function_documentation')
                 , title = 'Create function documentation'
                 , author = person('Andrew', 'Redd', email='andrew.redd@hsc.utah.edu')
                 , usage= call('function_documentation', as.name('name'), as.name('arguments'), as.name('usage'), as.name('...'))
                 , arguments = fun.args
                 , description = "create documentation for a function"
                 , value = "A function-Documentation object."
                 )
    expect_is(object, 'Documentation')
    expect_is(object, 'BaseDocumentation')
    expect_is(object, 'function-Documentation')

    expect_identical(object@name,as.name('function_documentation'))
    expect_identical(object@title, 'Create function documentation')
    expect_identical(object@author, person('Andrew', 'Redd', email='andrew.redd@hsc.utah.edu'))
    expect_identical(object@usage, as(expression(function_documentation(name, arguments, usage, ...)), 'usage'))
    expect_identical(object@arguments, fun.args)
    expect_identical(object@description, FT("create documentation for a function"))
    expect_identical(object@value, FT("A function-Documentation object."))

    object <- function_documentation()
    expect_true(.is_undefined(object@name))


    a <- arg('x', 'an object')
    object <- function_documentation(name='testing args', arguments = a)
    expect_identical(object@arguments, AL(a))

    docs2  <- function_documentation(name='testing args', arguments = list(a))
    expect_identical(docs2@arguments, AL(a))
    expect_identical(object, docs2)

    docs3  <- function_documentation(name='testing args', arguments = object)
    expect_identical(docs2@arguments, AL(a))
    expect_identical(object, docs2)
}
if(FALSE){#@testing documentation<-,function,function-Documentation
    hw <- function(){print("hello world")}
    documentation(hw) <- function_documentation(title = "the standard Hello world")

    docs <- documentation(hw)
    expect_is(docs, 'function-Documentation')
    expect_identical(docs, function_documentation(title = "the standard Hello world"))
    expect_true(.is_undefined(docs@name))
    expect_identical(docs@title, "the standard Hello world")
}

setAs('function-Documentation', 'ArgumentList', function(from)from@arguments)



### Class: S3method-Documentation #####
S3method_documentation <-
setClass( "S3method-Documentation"
        , contains = 'function-Documentation'
        , representation = representation( generic = 'name'
                                         , signature = 'name'
                                         )
        # , validity =
        )
setMethod('initialize', 'S3method-Documentation',
    function( .Object
            , generic
            , signature
            , ...
            , name=NULL
            , usage=NULL
            ){
    .Object@generic <- as.name(generic)
    .Object@signature <- as.name(signature)
    .Object <- callNextMethod( .Object, ...)
    .Object
})
if(FALSE){#@testing
    doc <- S3method_documentation('html_to_Rd', 'em')

    expect_identical(doc@generic, as.name('html_to_Rd'))
    expect_identical(doc@signature, as.name('em'))
}



