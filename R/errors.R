

doc_error <- function(msg, ..., type=NULL, call=sys.call(1)){
    class <- c( if (!is.null(type)) 'documentation-error-' %<<<% type
              , 'documentation-error', 'error', 'condition')
    cond <- structure(list(message = msg, call=call)
                     , ..., class=class)
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

doc_warning <- function(msg, ..., type=NULL, call=sys.call(1)){
    class <- c( if (!is.null(type)) "documentation-warning-" %<<<% type
              , 'documentation-warning', 'warning', 'condition'
              )
    cond <- structure( list(message = msg, call=call)
                     , class=class, ...)
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

doc_message <- function(msg, ..., type=NULL, call=sys.call(1)){
    class <- c( if (!is.null(type)) "documentation-message-" %<<<% type
              , 'documentation-message', 'message', 'condition'
              )
    cond <- structure( list(message = msg, call=call)
                     , class=class
                     , ...)
    message(cond)
}

doc_dnf_error <-
function(name=NULL, call=sys.call(1))
    doc_error( if (is.null(name)) ._("Documentation not found.")
               else ._("Documentation not found for '%s'.", name)
             , type='dnf', call=call)
if(FALSE){#@testing
    expect_error(doc_dnf_error(), "Documentation not found.")
    expect_error(doc_dnf_error("throw me"), "Documentation not found for 'throw me'.")
    x <- tryCatch( doc_dnf_error("hello")
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
    expect_identical(x, "Documentation is incomplete")
}

doc_no_src <-
function( name = NULL #< name of the object
        , cond = c('error', 'warning', 'message') #< type of condition to raise.
        , call = sys.call(1) #< call that produced error.
        ){
    cond <- match.arg(cond)
    switch(cond
          , error =
            doc_error(if (is.null(name)) ._("Object has no srcref.")
                      else ._("'%s' has no srcref.", name)
                     , type='no_src', call=call )
          , warning =
            doc_warning(if (is.null(name)) ._("Object has no srcref.")
                        else ._("'%s' has no srcref.", name)
                       , type='no_src', call=call )
          , message =
            doc_message(if (is.null(name)) ._("Object has no srcref.")
                        else ._("'%s' has no srcref.", name)
                       , type='no_src', call=call )
          )
}
if(FALSE){#@testing
    expect_error(doc_no_src(), class='documentation-error-no_src')
}

no_doc_comments <-
function( name = NULL #< name of the object
        , cond = c('message', 'error', 'warning') #< type of condition to raise.
        , call = sys.call(1) #< call that produced error.
        ){
    cond <- match.arg(cond)
    msg <- if (is.null(name)) ._("No documentation comments found.")
           else ._("No documentatino comments found for '%s'.", name)
    type <- no_comments
    switch( cond
          , error   = doc_error  (msg, type=type, call=call )
          , warning = doc_warning(msg, type=type, call=call)
          , message = doc_message(msg, type=type, call=call)
          )
}


