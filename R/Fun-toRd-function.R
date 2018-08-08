#' @include Classes.R
#' @include Fun-toRd.R


setMethod('toRd', 'function-Documentation',
function( obj
        , ...
        ){
    #' format the function documentation obj to markdown/CommonMark
    Rd <- callNextMethod()
    Rd[['arguments']] <- collapse(toRd(obj@arguments), '\n')
    Rd[['usage']]     <- Rd_tag(deparse(obj@usage), 'usage')

    if (is.na(obj@value))
        Rd[['value']] <- ''
    Rd <- Rd[nchar(Rd)>0]

    return(Rd)
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
    Rd <- toRd(obj)
    expect_true(.valid_Rd(Rd))
    expect_true(all(c('name', 'usage', 'value', 'arguments') %in% names(Rd)))
    expect_equal(Rd[['name']], '\\name{function_documentation}')
    expect_equal(Rd[['value']], '\\value{A function-Documentation obj.}')
    expect_equal(Rd[['usage']], '\\usage{function_documentation(name, arguments, usage, ...)}')
    expect_equal(length(Rd[['arguments']]), 1)

    expect_false(any(nchar(Rd) == 0))


    doc <- function_documentation( name = 'test'
                                 , title = "test title"
                                 , description = "A description for my test function"
                                 , arguments = ArgumentList( arg_('x', 'the x argument'))
                                 )
    Rd <- toRd(doc)
    expect_false(any(nchar(Rd) == 0))
}
