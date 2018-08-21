#' @include Classes.R
#' @include Fun-accessors.R

setMethod("doc_get_usage", "function-Documentation", function(doc){
    if (is(doc@usage, 'waiver')){
        as(as.call( c( as.name(doc_get_name(doc))
                     , lapply(doc_get_arguments(doc), slot, 'name')
                     )), 'usage')
    } else doc@usage
})
if(FALSE){#@testing
    doc <- function_documentation( name=as.name('test')
                                 , arguments = AL( arg_('x', 'first')
                                                 , arg_('y', 'second')
                                                 )
                                 , usage = usage(expression(test(x,y,...)))
                                 )
    expect_is(doc@usage, 'usage')
    expect_identical(doc@usage, usage(expression(test(x,y,...))))
    expect_identical(doc_get_usage(doc), usage(expression(test(x,y,...))))

    doc <- function_documentation( name=as.name('test')
                                 , arguments = AL( arg_('x', 'first')
                                                 , arg_('y', 'second')
                                                 )
                                 , usage = substitute(test(x,y,...))
                                 )
    expect_is(doc@usage, 'usage')
    expect_identical(doc@usage, usage(expression(test(x,y,...))))
    expect_identical(doc_get_usage(doc), usage(expression(test(x,y,...))))

    doc <- function_documentation( name=as.name('test')
                                 , arguments = AL( arg_('x', 'first')
                                                 , arg_('y', 'second')
                                                 )
                                 , usage = expression(test(x,y,...))
                                 )
    expect_is(doc@usage, 'usage')

    doc <- function_documentation( name=as.name('test')
                                 , arguments = AL( arg_('x', 'first')
                                                 , arg_('y', 'second')
                                                 )
                                 , usage = usage_waiver()
                                 )

    expect_is(doc@usage, 'usage-waiver')
    expect_is(doc@usage, 'waiver')
    u <- doc_get_usage(doc)
    expect_is(u, 'usage')
    expect_identical(u, usage(expression(test(x,y))))
}
