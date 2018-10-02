#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Fun-accessors-funtion.R`')
#line 12 "R/Fun-accessors-funtion.R"
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
