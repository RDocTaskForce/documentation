#' @include Classes.R
#' @include Fun-accessors.R


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
if(FALSE){#@testing Shared accessors
    doc <- shared(function_documentation())
    expect_is_exactly(doc, 'Shared-function-Documentation')
    doc_name(doc) <- 'Normal'
    expect_is_exactly(doc, 'Shared-function-Documentation')

    expect_identical(doc_get_name(doc), 'Normal')
    expect_identical(doc$docs@name, substitute(Normal))
}

### Method: doc_has_name,Shared-Documentation #####
setMethod('doc_has_name', 'Shared-Documentation', function(doc){
    !.is_undefined(doc$docs@name)
})
if(FALSE){#@testing
    doc <- shared(function_documentation())
    expect_false(doc_has_name(doc))
    doc_name(doc) <- "test"
    expect_true(doc_has_name(doc))
}

