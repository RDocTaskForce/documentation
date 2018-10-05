
### Generate accessors for Shared-Documentation #####
for (fname in objects( envir=topenv(environment(doc_get_aliases))
                     , pattern = "^doc_get_"
                     )) {
    setMethod(fname, signature = c('Shared-Documentation'),
        s( eval(substitute( function(doc)fun(doc$docs)
                          , list(fun=rlang::sym(fname))))
         , srcref=NULL)
    )
    setter.name <- gsub('^(doc_)(get_)(.+)$', "\\1\\3<-", fname)
    slot.name <- gsub('^(doc_)(get_)(.+)$', "\\3", fname)
    setMethod(setter.name, signature = c('Shared-Documentation'),
        s( eval(substitute( function(doc, value){
                                doc$docs <- fun(doc$docs, value)
                                return(invisible(doc))
                            }
                          , list( fun  = rlang::sym(setter.name)
                                , slot = rlang::sym(slot.name)
                                )))
         , srcref=NULL)
    )

}
if(FALSE){#@testing
    doc <- shared(docs=function_documentation())
    expect_is_exactly(doc, 'Shared-Documentation')
    doc_name(doc) <- 'Normal'
    expect_is_exactly(doc, 'Shared-Documentation')

    expect_identical(doc_get_name(doc), 'Normal')
    expect_identical(doc$docs@name, substitute(Normal))
}




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
}

