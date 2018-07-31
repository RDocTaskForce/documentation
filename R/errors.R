

doc_error <- function(msg, type=NULL, call=sys.call(1)){
    class <- c( if (!is.null(type)) 'documentation-error-' %<<<% type
              , 'documentation-error', 'error', 'condition')
    cond <- structure(list(message = msg, call=call)
                     , class=class)
    stop(cond)
}
if(FALSE){#@testing
    expect_error(doc_error("A documentation error."), "A documentation error.")
    x <- tryCatch( doc_error("A documentation error.")
                 , 'documentation-error-dnf'= function(e)
                        "Documentation not found!"
                 , 'documentation-error-invalid'= function(e)
                        "Documentation is invalid!"
                 , 'documentation-warning-incomplete'= function(w)
                        "Documentation is incomplete"
                 , 'documentation-error' = function(e)
                        "General documentation error."
                 , 'documentation-warning' = function(e)
                        "General documentation warning."
                 )
    expect_identical(x, "General documentation error.")
}

doc_warning <- function(msg, type=NULL, call=sys.call(1)){
    class <- c( if (!is.null(type)) "documenation-warning-" %<<<% type
              , 'documentation-warning', 'warning', 'condition'
              )
    cond <- structure(list(message = msg, call=call)
                     , class=class)
    warning(cond)
}
if(FALSE){#@testing
    expect_warning(doc_warning("A documentation warning"), "A documentation warning")
    x <- tryCatch( doc_warning("A documentation warning.")
                 , 'documentation-error-dnf'= function(e)
                        "Documentation not found!"
                 , 'documentation-error-invalid'= function(e)
                        "Documentation is invalid!"
                 , 'documentation-warning-incomplete'= function(w)
                        "Documentation is incomplete"
                 , 'documentation-error' = function(e)
                        "General documentation error."
                 , 'documentation-warning' = function(e)
                        "General documentation warning."
                 )
    expect_identical(x, "General documentation warning.")
}

doc_dnf_error <- 
function(name=NULL, call=sys.call(1))
    doc_error( if (is.null(name)) ._("Documentation not found.")
               else ._("Documentation not found for '%s'.", name)
             , type='dnf', call=call)
if(FALSE){#@testing
    expect_error(doc_dnf_error(), "Documentation not found.")
    expect_error(doc_dnf_error("throw me"), "Documentation not found for 'throw me'.")
    x <- tryCatch( doc_incomplete("hello")
                 , 'documentation-error-dnf'= function(e)
                        "Documentation not found!"
                 , 'documentation-error-invalid'= function(e)
                        "Documentation is invalid!"
                 , 'documentation-warning-incomplete'= function(w)
                        "Documentation is incomplete"
                 , 'documentation-error' = function(e)
                        "General documentation error."
                 , 'documentation-warning' = function(e)
                        "General documentation warning."
                 )
    expect_identical(x, "Documentation not found!")
}
doc_invalid <- function(name=NULL, call=sys.call(1))
    doc_error( if (is.null(name)) ._("Documentation is not valid.") 
               else ._("Documentation for '%s' is not valid.", name)
             , type='invalid', call=call)
if(FALSE){#@testing
    expect_error(doc_invalid(), "Documentation is not valid.")
    expect_error(doc_invalid("throw me"), "Documentation for 'throw me' is not valid.")
    x <- tryCatch( doc_invalid("hello")
                 , 'documentation-error-dnf'= function(e)
                        "Documentation not found!"
                 , 'documentation-error-invalid'= function(e)
                        "Documentation is invalid!"
                 , 'documentation-warning-incomplete'= function(w)
                        "Documentation is incomplete"
                 , 'documentation-error' = function(e)
                        "General documentation error."
                 , 'documentation-warning' = function(e)
                        "General documentation warning."
                 )
    expect_identical(x, "Documentation is invalid!")
}

doc_incomplete <- function(name=NULL, call=sys.call(1))
    doc_warning( if (is.null(name)) ._("Documentation is incomplete.")
                 else ._("Documentation is incomplete for '%s'.", name)
               , type='incomplete', call=call)
if(FALSE){#@testing
    expect_warning(doc_incomplete(), "Documentation is incomplete.")
    expect_warning(doc_incomplete("hello"), "Documentation is incomplete for 'hello'.")
    x <- tryCatch( doc_incomplete("hello")
                 , 'documentation-error-dnf'= function(e)
                        "Documentation not found!"
                 , 'documentation-error-invalid'= function(e)
                        "Documentation is invalid!"
                 , 'documentation-warning-incomplete'= function(w)
                        "Documentation is incomplete"
                 , 'documentation-error' = function(e)
                        "General documentation error."
                 , 'documentation-warning' = function(e)
                        "General documentation warning."
                 )
    expect_identical(x, "Documentation is invalid!")
}
