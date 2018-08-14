#' @include util-aliases.R

.conditions <- c('message', 'warning', 'error', 'none')

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

doc_condition <- function(msg, cond, ..., type=NULL, call = sys.call(1)){
    if (is.null(cond)) return()
    if (is.logical(cond))
        cond <- if (is.na(cond)) 'warning'
                else if (cond) 'error'
                else 'message'

    cond <- match.arg(cond, choices = .conditions)
    switch( cond
          , error   = doc_error  (msg, ..., type=type, call=call)
          , warning = doc_warning(msg, ..., type=type, call=call)
          , message = doc_message(msg, ..., type=type, call=call)
          )
}


doc_dnf_error <-
function(name=NULL, call=sys.call(1)){
    doc_error( if (is.null(name)) ._("Documentation not found.")
               else ._("Documentation not found for '%s'.", as.character(name))
             , name=name
             , type='dnf', call=call)
}
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
doc_invalid <- function(name=NULL, call=sys.call(1)){
    doc_condition( if (is.null(name)) ._("Documentation is not valid.")
                   else ._("Documentation for '%s' is not valid.", as.character(name))
                 , cond='error'
                 , name=name
                 , type='invalid', call=call)
}
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

doc_incomplete <- function(name=NULL, cond='warn', call=sys.call(1)){
    msg <- if (is.null(name)) ._("Documentation is incomplete.")
           else ._("Documentation is incomplete for '%s'.", as.character(name))
    doc_condition( msg, cond, name=name, type='incomplete', call=call)
}
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
function( name = NULL     #< name of the object
        , cond = 'error'  #< type of condition to raise.
        , call = sys.call(1) #< call that produced error.
        ){
    msg <- if (is.null(name)) ._("Object has no srcref.")
           else ._("'%s' has no srcref.", as.character(name))
    doc_condition(msg, cond, name=name, type='no_src', call=call)
}
if(FALSE){#@testing
    expect_error(doc_no_src(), class='documentation-error-no_src')
}

no_doc_comments <-
function( name = NULL #< name of the object
        , cond = 'message' #< type of condition to raise.
        , call = sys.call(1) #< call that produced error.
        ){
    msg <- if (is.null(name)) ._("No documentation comments found.")
           else ._("No documentation comments found for '%s'.", as.character(name))
    doc_condition(msg, cond, name=name, type = "no_comments", call=call)
}

doc_overwrite <- function(name = NULL, cond = 'message', call=sys.call(1)){
    doc_condition( if (is.null(name)) ._("Object alreay has documentation.")
                   else ._("Object %s already has documentation", as.character(name))
                 , cond=cond, type='overwrite', call=call
                 , name=name)
}


