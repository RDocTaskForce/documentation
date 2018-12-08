


### Method: set_documentation_,function,Shared-Documentation #####
setMethod('set_documentation_', signature = c('function', 'Shared-Documentation'),
    function( object, doc
            , name
            , envir
            , ...
            , .infer=TRUE
            ){
        assert_that( is.flag(.infer))
        args <- list(...)

        if (.infer){
            if (.is_undefined(doc$docs@name))
                doc$docs@name <- name
            if (!length(. <- doc_get_aliases(doc)) || deparse(name) %!in% .)
                doc$docs@aliases <- c(., deparse(name))
        }
        if ('usage' %in% names(args)) {
            doc$docs@usage <- c(doc_get_usage, args$usage)
            args$usage <- NULL
        } else {
            doc$docs@usage <- c( doc_get_usage(doc)
                               , as(object, 'usage'))
        }
        for (i in seq_along(args)){
            doc_error("not implimented")
        }
        attr(object, 'documentation') <- doc
        assign(deparse(name), value=object, envir = envir)
        invisible(doc)
    })
if(FALSE){#@testing
    doc <- shared(function_documentation( name = 'Normal'
                                        , title = 'The Normal Distribution'
                                        ))

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
}

