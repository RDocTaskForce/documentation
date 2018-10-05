#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Fun-accessors-function.R`')
#line 13 "/rdtf/documentation/R/Fun-accessors-function.R"
test_that('doc_get_usage,function-Documentation-method', {#@testing
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
})
#line 61 "/rdtf/documentation/R/Fun-accessors-function.R"
test_that('doc_get_name,S3method-Documentation-method', {#@testing
    doc <- S3method_documentation('html_to_Rd', 'em')

    expect_identical(doc@generic, as.name('html_to_Rd'))
    expect_identical(doc@signature, as.name('em'))
    expect_identical(doc@name, .undefined)
    expect_identical(doc_get_name(doc), 'html_to_Rd.em')
})
#line 74 "/rdtf/documentation/R/Fun-accessors-function.R"
test_that('doc_get_aliases,function-Documentation-method', {#@testing
    doc <- function_documentation( name = "Normal"
                                 , title = "The Normal Distribution"
                                 , aliases = c('rnorm', 'dnorm', 'pnorm', 'qnorm')
                                 )
    expect_identical(doc_get_aliases(doc), .T(Normal, dnorm, pnorm, qnorm, rnorm))
})
