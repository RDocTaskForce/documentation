#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Fun-documentation-Shared.R`')
#line 24 "R/Fun-documentation-Shared.R"
test_that('Shared accessors', {#@testing Shared accessors
    doc <- shared(docs=function_documentation())
    expect_is_exactly(doc, 'Shared-Documentation')
    doc_name(doc) <- 'Normal'
    expect_is_exactly(doc, 'Shared-Documentation')

    expect_identical(doc_get_name(doc), 'Normal')
    expect_identical(doc$docs@name, substitute(Normal))
})
#line 68 "R/Fun-documentation-Shared.R"
test_that('set_documentation_,function,Shared-Documentation-method', {#@testing
    doc <- new( 'Shared-Documentation'
              , docs = function_documentation( name = 'Normal'
                                             , title = 'The Normal Distribution'
                                             )
              )

    rnorm <- stats::rnorm
    dnorm <- stats::dnorm

    expect_is(doc, 'Shared-Documentation')
    set_documentation(rnorm, doc, envir = environment())
    expect_is(documentation(rnorm), 'Shared-Documentation')
    expect_identical(documentation(rnorm), doc)

    expect_equal(doc_get_aliases(doc), .T('Normal', 'rnorm'))
    expect_equal(doc_get_name(doc), 'Normal')

    set_documentation(dnorm, doc, envir = environment())
    expect_is(documentation(dnorm), 'Shared-Documentation')
    expect_identical(documentation(dnorm), doc)
    expect_identical(documentation(rnorm), doc)

    expect_equal(doc_get_aliases(doc), .T(Normal, dnorm, rnorm))
    expect_equal(doc_get_name(doc), 'Normal')
})
