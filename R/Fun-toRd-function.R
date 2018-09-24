#' @include Classes.R
#' @include Fun-toRd.R


.default.functiondocumentation.order <-
    c( 'name', 'aliases', 'concepts', 'title', 'author'
     , 'description', 'usage', 'arguments'
     , 'sections'
     , 'value', 'seealso'
     , 'references'
     , 'examples'
     , 'keywords'
     )

setMethod('toRd', 'function-Documentation',
function( obj
        , ...
        , control = list()
        ){
    #' format the function documentation obj to Rd format.
    rd <- callNextMethod(obj, exclude=c('usage','arguments'), ...)
    rd <- c(rd, toRd(doc_get_arguments(obj), ...))
    rd <- c(rd, toRd(doc_get_usage(obj), ...))

    order <- get_option( "Documentation::function-Documentation::documentation-order"
                       , .default.functiondocumentation.order
                       )
    if (is.na(obj@value))
        rd[['value']] <- ''
    rd <- rd[nchar(rd)>0]

    return(rd)
})
if(FALSE){#! @testing
    obj <- new( "function-Documentation"
              , name = as.name('function_documentation')
              , title = 'Create function documentation'
              , author = person('Andrew', 'Redd', email='andrew.redd@hsc.utah.edu')
              , usage= call('function_documentation', as.name('name'), as.name('arguments'), as.name('usage'), as.name('...'))
              , arguments = ArgumentList( arg(name     , "Name of the function")
                                        , arg(arguments, "Argument list"               , class="ArgumentList")
                                        , arg(value    , "Return value of the function")
                                        , arg(usage    , "Usage string to override default, constructed from the name and arguments.", class="call")
                                        , arg('...'    , "other arguments to contruct the Documentation obj.")
                                        )
              , value = "A function-Documentation obj."
              )
    rd <- toRd(obj)
    expect_is_exactly(rd, 'Rd')

    expect_equal(rd[['\\name']], Rd_name('function_documentation'))
    expect_equal(rd[['\\value']], '\\value{A function-Documentation obj.}')
    expect_equal(rd[['\\usage']], '\\usage{function_documentation(name, arguments, usage, ...)}')
    expect_equal(length(rd[['\\arguments']]), 1)

    expect_false(any(nchar(Rd) == 0))


    doc <- function_documentation( name = 'test'
                                 , title = "test title"
                                 , description = "A description for my test function"
                                 , arguments = ArgumentList( arg_('x', 'the x argument'))
                                 )
    Rd <- toRd(doc)
    expect_false(any(nchar(Rd) == 0))
}
