#' @include Classes.R
#' @include Fun-accessors.R

#' @export
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

### doc_get_name, S3Method-Documentation
#' @export
setMethod("doc_get_name", "S3method-Documentation", function(doc){
    as.name(paste0(doc@generic, '.', doc@signature))
})
if(FALSE){#@testing
    doc <- S3method_documentation('html_to_Rd', 'em')

    expect_identical(doc@generic, as.name('html_to_Rd'))
    expect_identical(doc@signature, as.name('em'))
    expect_identical(doc@name, .undefined)
    expect_identical(doc_get_name(doc), as.name('html_to_Rd.em'))
}


setMethod("doc_get_aliases", "function-Documentation", function(doc){
    union(doc_get_name(doc), sort(doc@aliases))
})
if(FALSE){#@testing
    doc <- function_documentation( name = "Normal"
                                 , title = "The Normal Distribution"
                                 , aliases = c('rnorm', 'dnorm', 'pnorm', 'qnorm')
                                 )
    expect_identical(doc_get_aliases(doc), .T(Normal, dnorm, pnorm, qnorm, rnorm))
}