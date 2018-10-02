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

#' format the function documentation obj to Rd format.
#' @export
setMethod('toRd', 'function-Documentation',
function( obj
        , ...
        , control = list()
        , raw.list = FALSE
        ){

    order <- get_option( "Documentation::function-Documentation::documentation-order"
                       , .default.functiondocumentation.order
                       )
    order <- unique(c(order, slotNames(obj)))
    order <- setdiff(order, 'exclude')

    rd <- callNextMethod(obj, exclude=c('usage','arguments'), ..., raw.list=TRUE)
    rd$name      <- Rd_name(doc_get_name(obj))
    rd$arguments <- toRd(doc_get_arguments(obj), ..., control=control)
    rd$usage     <- toRd(doc_get_usage(obj), ..., control=control)
    rd$value     <- Rd_value(toRd(doc_get_value(obj))) %if% !is.na(doc_get_value(obj))

    order <- intersect(order, names(rd))
    rd <- rd[order]
    rd <- rd[nchar(rd)>0]

    if (raw.list) return(rd)

    Rd_lines(rd)
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
    expect_equal(rd[['\\value']], Rd_tag('value', Rd_text('A function-Documentation obj.')))
    expect_equal(rd[['\\usage']], Rd_usage(Rd_rcode('function_documentation(name, arguments, usage, ...)')))
    expect_equal(rd[['\\arguments']]
                , Rd_arguments( Rd_item('name', "Name of the function")
                              , Rd_item('arguments', "Argument list")
                              , Rd_item('value'    , "Return value of the function")
                              , Rd_item('usage'    , "Usage string to override default, constructed from the name and arguments.")
                              , Rd_item('...'    , "other arguments to contruct the Documentation obj.")
                              , indent=FALSE))
    expect_false(any(nchar(rd) == 0))

    expect_rd_output(rd, "Fun-toRd-function.Rd", 'toRd,function-Documenation')
}
