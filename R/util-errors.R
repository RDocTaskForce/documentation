#' @include util-aliases.R
#' @import pkgcond

test_doc_condition <- function(fun, ...){
    fun <- match.fun(fun)
    fun(...)
}

doc_error <- function(msg, ..., type=NULL, call=sys.call(1)){
    condition(msg, 'error', ..., type=type, call=call)
}
if(FALSE){#@testing
    expect_error(doc_error("A documentation error."), "A documentation error.")
    x <- catch_condition(test_doc_condition(doc_error, "A documentation error."))
    expect_is(x, "documentation::test_doc_condition-error")
}

doc_warning <- function(msg, ..., type=NULL, call=sys.call(1)){
    condition(msg, 'warning', ..., type=type, call=call)
}
if(FALSE){#@testing
    expect_warning(doc_warning("A documentation warning"), "A documentation warning")
    x <- catch_condition(test_doc_condition(doc_warning, "A documentation warning."))
    expect_is(x, "documentation::test_doc_condition-warning")
}

doc_message <- function(msg, ..., type=NULL, call=sys.call(1)){
    condition(msg, 'message', ..., type=type, call=call)
}
if(FALSE){#@testing
    expect_message(doc_message("A documentation message"), "A documentation message")
    x <- catch_condition(test_doc_condition(doc_message, "A documentation message"))
    expect_is(x, "documentation::test_doc_condition-message")
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
    x <- tryCatch( test_doc_condition(doc_dnf_error, "hello")
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
    condition( if (is.null(name)) ._("Documentation is not valid.")
               else ._("Documentation for '%s' is not valid.", as.character(name))
             , cond='error'
             , name=name
             , type='invalid', call=call)
}
if(FALSE){#@testing
    expect_error(doc_invalid(), "Documentation is not valid.")
    expect_error(doc_invalid("throw me"), "Documentation for 'throw me' is not valid.")
    x <- tryCatch( test_doc_condition(doc_invalid, "hello")
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
    condition( msg, cond, name=name, type='incomplete', call=call)
}
if(FALSE){#@testing
    expect_warning(doc_incomplete(), "Documentation is incomplete.")
    expect_warning(doc_incomplete("hello"), "Documentation is incomplete for 'hello'.")
    x <- tryCatch( test_doc_condition(doc_incomplete, "hello")
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
    condition(msg, cond, name=name, type='no_src', call=call)
}
if(FALSE){#@testing
    expect_error(test_doc_condition(doc_no_src), class='documentation-error-no_src')
}

no_doc_comments <-
function( name = NULL #< name of the object
        , cond = 'message' #< type of condition to raise.
        , call = sys.call(1) #< call that produced error.
        ){
    msg <- if (is.null(name)) ._("No documentation comments found.")
           else ._("No documentation comments found for '%s'.", as.character(name))
    condition(msg, cond, name=name, type = "no_comments", call=call)
}
if(FALSE){#@testing
    expect_null(test_doc_condition(no_doc_comments, 'testing', NULL))

    expect_null(test_doc_condition(no_doc_comments, 'testing', 'none'))
    expect_message( test_doc_condition(no_doc_comments, 'testing', 'message')
                  , class="documentation-message-no_comments")
    expect_warning( test_doc_condition(no_doc_comments, 'testing', 'warning')
                  , class="documentation-warning-no_comments")
    expect_error  ( test_doc_condition(no_doc_comments, 'testing', 'error')
                  , class="documentation-error-no_comments")
}


doc_overwrite <- function(name = NULL, cond = 'message', call=sys.call(1)){
    condition( if (is.null(name)) ._("Object alreay has documentation.")
               else ._("Object %s already has documentation", as.character(name))
             , cond=cond, type='overwrite', call=call
             , name=name)
}

klass <- function(x)collapse(class(x), '/')

doc_error_bad_argument <-
function( obj
        , expected
        , ...
        , msg = "Invalid `{arg.name}` argument to `{fun.name}`;" %<<%
                "expected a {sQuote(expected)}," %<<%
                "received a {sQuote(klass(obj))}."
        , arg.name = deparse(substitute(obj))
        , fun.name = deparse(sys.call(-1)[[1]])
        , scope= c('documentation', fun.name)
        , type = 'invalid_argument'
        , cond = 'error'
        ){
    msg <- gettext(msg)
    msg <- glue::glue( msg)

    condition(msg, ...,  cond = cond, type=type)
}
if(FALSE){#@testing
    f <- function(a)
        doc_error_bad_argument(a, 'logical')
    expect_error( f('hi'), class = "documentation-error-invalid_argument")

}

# Skip Scope -----
doc_error <- skip_scope(doc_error)
doc_warning <- skip_scope(doc_warning)
doc_message <- skip_scope(doc_message)
doc_dnf_error <- skip_scope(doc_dnf_error)
doc_invalid <- skip_scope(doc_invalid)
doc_incomplete <- skip_scope(doc_incomplete)
doc_no_src <- skip_scope(doc_no_src)
no_doc_comments <- skip_scope(no_doc_comments)
doc_overwrite <- skip_scope(doc_overwrite)
doc_error_bad_argument <- skip_scope(doc_error_bad_argument)
