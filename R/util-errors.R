#' @include util-aliases.R

.conditions <- c('message', 'warning', 'error', 'none')

doc_error <- function(msg, ..., type=NULL, call=sys.call(1)){
    class <- c( paste0('documentation-error-', type) %if% (!is.null(type))
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

#' Raise a mutable and classed condition.
condition <-
function( msg
        , cond = .conditions
        , ... #< objects to be added to the condition as attributes.
        , scope = environmentName(topenv(parent.frame()))
        , type = NULL #< optional type of the condition, used to create the class.
        , call = sys.call(1)
        ){
    cond = match.arg(cond)
    if (cond == 'none') return()

    throw <- function(ball){
        if (is(ball, 'error')) stop(ball) else
        if (is(ball, 'warning')) warning(ball) else
        if (is(ball, 'message')) message(ball)
    }
    while (length(scope) && scope[[1]] %in% c("", "R_GlobalEnv", "base"))
        scope <- scope[-1L]
    if (length(scope) > 1L)
        scope <- purrr::accumulate(scope, paste, sep='::')
    classes <-{
        c( c( paste0(scope, '-', cond, '-', type) %if% !is.null(type)
            , paste0(scope, '-', cond)
            , paste0(scope, '-condition')
            ) %if% length(scope)
         , paste0(cond, '-', type) %if% !is.null(type)
         , cond
         , 'condition'
         )
    }
    ball <- structure( list(message = msg, call=call)
                     , class=classes
                     , ...)
    throw(ball)
}
if(FALSE){#@testing
    expect_message( condition('testing', 'message', scope='base'), 'testing')
    expect_message( condition('testing', 'message', scope='base', type='testing')
                  , class = "message-testing"
                  )
    expect_message( condition('testing', 'message', scope='test', type='testing')
                  , class = "test-message-testing"
                  )

    expect_warning( condition('testing', 'warning', scope='base'), 'testing')
    expect_warning( condition('testing', 'warning', scope='base', type='testing')
                  , class = "warning-testing"
                  )
    expect_warning( condition('testing', 'warning', scope='test', type='testing')
                  , class = "test-warning-testing"
                  )

    expect_error( condition('testing', 'error', scope='base'), 'testing')
    expect_error( condition('testing', 'error', scope='base', type='testing')
                , class = "error-testing"
                )
    expect_error( condition('testing', 'error', scope='test', type='testing')
                , class = "test-error-testing"
                )

    tryCatch( condition('testing', 'error', type='testing'
                       , scope = .T(test, my_class, my_method)
                       )
            , condition = function(obj){
                expect_is(obj, 'test-error-testing')
                expect_is(obj, 'test::my_class-error-testing')
                expect_is(obj, 'test::my_class::my_method-error-testing')
                expect_is(obj, 'test-error')
                expect_is(obj, 'test::my_class-error')
                expect_is(obj, 'test::my_class::my_method-error')
                expect_is(obj, 'error-testing')
                expect_is(obj, 'error')
                expect_is(obj, 'condition')
            })
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
if(FALSE){#@testing
    expect_null(doc_condition("test message", NULL))

    expect_message( doc_condition("test message", FALSE, type='testing')
                  , class = "documentation-message-testing" )
    expect_warning( doc_condition("test message", NA   , type='testing')
                  , class = "documentation-warning-testing" )
    expect_error  ( doc_condition("test message", TRUE , type='testing')
                  , class = "documentation-error-testing" )

    expect_null(doc_condition("test message", 'none'))
    expect_message( doc_condition("test message", 'message', type='testing')
                  , class = "documentation-message-testing" )
    expect_warning( doc_condition("test message", 'warning', type='testing')
                  , class = "documentation-warning-testing" )
    expect_error  ( doc_condition("test message", 'error'  , type='testing')
                  , class = "documentation-error-testing" )
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
if(FALSE){#@testing
    expect_null(no_doc_comments('testing', NULL))
    expect_message( no_doc_comments('testing', FALSE)
                  , class="documentation-message-no_comments")
    expect_warning( no_doc_comments('testing', NA)
                  , class="documentation-warning-no_comments")
    expect_error  ( no_doc_comments('testing', TRUE)
                  , class="documentation-error-no_comments")

    expect_null(no_doc_comments('testing', 'none'))
    expect_message( no_doc_comments('testing', 'message')
                  , class="documentation-message-no_comments")
    expect_warning( no_doc_comments('testing', 'warning')
                  , class="documentation-warning-no_comments")
    expect_error  ( no_doc_comments('testing', 'error')
                  , class="documentation-error-no_comments")
}


doc_overwrite <- function(name = NULL, cond = 'message', call=sys.call(1)){
    doc_condition( if (is.null(name)) ._("Object alreay has documentation.")
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
                     # , obj=obj, expected=expected
                     # , arg.name = arg.name, fun.name=fun.name
                     # )
    condition(msg, ...,  cond = cond, scope = scope, type=type)
}
if(FALSE){#@testing
    f <- function(a){
        doc_error_bad_argument(a, 'logical')
    }
    expect_error( f('hi'), class = "documentation-error-invalid_argument")

}