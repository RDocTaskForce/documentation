expect_is_not <-
function (object, class, info = NULL, label = NULL){
    stopifnot(is.character(class))
    act <- testthat::quasi_label(rlang::enquo(object), label)
    act$class <-
    exp_lab <- paste(class, collapse = "/")
    testthat::expect( Negate(inherits)(act$val, class)
                    , sprintf("%s is a %s; should not inherit from `%s`."
                             , act$lab, act$class, exp_lab)
                    , info = info)
    invisible(act$val)
}

expect_valid <-
function (object, complete=FALSE, info=NULL, label=NULL){
    act <- testthat::quasi_label(rlang::enquo(object), label)
    is.valid <- validObject(object, test=TRUE, complete=complete)
    testthat::expect(isTRUE(is.valid)
                    , ._("%s is not valid; %s", act$lab, dQuote(is.valid))
                    , info=info
                    )
}
if(FALSE){#@testing
    bad <- s(1L, class='Rd', Rd_tag='integer')
    expect_error( expect_valid(bad)
                , "`bad` is not valid;" %<<% dQuote("object is not a list")
                )

    good <- s(list(), class='Rd')
    expect_valid(good)
}


class0 <- function(x)collapse(class(x), '/')

all_inherit <- function(lst, what, label=NULL){
    act <- testthat::quasi_label(rlang::enquo(lst), label)
    stopifnot( is.character(what) || is.null(what) )
    if (all(. <- purrr::map_lgl(lst, inherits, what=what, which=FALSE)))
        return(TRUE)
    msg <- if (sum(!.) > 1L) {
        ._("%s has bad elements at %s which do not inherit from %s."
          , act$lab
          , comma_list(which(!.))
          , comma_list(dQuote(what), sep2 = ' or ', sep.last = ' or ')
          ) } else {
        bad.class <- purrr::map_chr(lst[!.], class0)
        ._("%s has bad element at %s which does not inherit from %s. It is a %s"
          , act$lab
          , comma_list(which(!.))
          , comma_list(dQuote(what), sep2 = ' or ', sep.last = ' or ')
          , dQuote(bad.class)
          )
          }
    return(s(FALSE, msg))
}
if(FALSE){#@ testing
    l <- list( 'a', 'b', 'c'
             , 1, 2
             , function()"hello world"
             )
    expect_identical( validate_that(all_inherit(l, 'character'))
                    , "`l` has bad elements at 4, 5, and 6" %<<%
                      "which do not inherit from" %<<%
                      dQuote("character") %<<<% '.')
    expect_identical( validate_that(all_inherit(l, c('character', 'function')))
                    , "`l` has bad elements at 4 and 5" %<<%
                      "which do not inherit from" %<<%
                      dQuote("character") %<<% 'or' %<<%
                      dQuote("function") %<<<% '.')
    expect_identical( validate_that(all_inherit(l, c('character', 'numeric')))
                    , "`l` has bad element at 6" %<<%
                      "which does not inherit from" %<<%
                      dQuote("character") %<<% 'or' %<<%
                      dQuote("numeric") %<<<% '.' %<<%
                      "It is a" %<<% dQuote("function"))

    expect_true( all_inherit(list(1L, 2L, 3L), 'integer'))
}
expect_all_inherit <- function (object, class, info = NULL, label = NULL) {
    act <- testthat::quasi_label(rlang::enquo(object), label)
    test <- all_inherit(object, class, label=act$lab)
    testthat::expect( isTRUE(test)
                    , attr(test, 'msg')
                    , info = info)
    invisible(test)
}
if(FALSE){
    expect_true( expect_all_inherit(1:3, 'integer'))
    l <- list( 'a', 'b', 'c'
             , 1, 2
             , function()"hello world"
             )
    expect_error( expect_all_inherit(l, 'character')
                , "`l` has bad elements at 4, 5, and 6" %<<%
                  "which do not inherit from" %<<%
                  dQuote("character") %<<<% '.')
    expect_error( expect_all_inherit(l, c('character', 'function'))
                , "`l` has bad elements at 4 and 5" %<<%
                  "which do not inherit from" %<<%
                  dQuote("character") %<<% 'or' %<<%
                  dQuote("function") %<<<% '.')
    expect_error( expect_all_inherit(l, c('character', 'numeric'))
                , "`l` has bad element at 6" %<<%
                  "which does not inherit from" %<<%
                  dQuote("character") %<<% 'or' %<<%
                  dQuote("numeric") %<<<% '.' %<<%
                  "It is a" %<<% dQuote("function"))


}


all_are <- function(lst, what, label=NULL, exact=TRUE){
    act <- testthat::quasi_label(rlang::enquo(lst), label)
    stopifnot( is.string(what) )
    if (!exact) return(all_inherit(lst, what, label=act$label))
    if (all(. <- purrr::map_int(lst, inherits, what, which=TRUE) == 1L))
        return(TRUE)
    bad.class <- purrr::map_chr(lst[!.], class0)
    msg <- if (sum(!.) > 1L){
        ._("%s has bad elements at positions %s which are not of class %s."
          , act$lab
          , comma_list(which(!.))
          , dQuote(what)
          )} else {
        ._("%s has bad element at position %s which is not of class %s."
          , act$lab
          , which(!.)
          , dQuote(what)
        )}
    return(s(FALSE, msg))
}
if(FALSE){#@testing
    l <- list( 'a', 'b', 'c'
             , 1, 2)
    expect_identical( validate_that(all_are(l, 'character'))
                    , "`l` has bad elements at positions 4 and 5" %<<%
                      "which are not of class" %<<%
                      dQuote("character") %<<<% '.')
    expect_identical( validate_that(all_are(list(1,2), 'integer', '...'))
                    , "... has bad elements at positions 1 and 2" %<<%
                      "which are not of class" %<<%
                      dQuote("integer") %<<<% '.')
    expect_identical( validate_that(all_are(list(1L,2L), 'numeric', '...'))
                    , "... has bad elements at positions 1 and 2" %<<%
                      "which are not of class" %<<%
                      dQuote("numeric") %<<<% '.')
    expect_identical( validate_that(all_are(list(1, 2L), 'numeric', '...'))
                    , "... has bad element at position 2" %<<%
                      "which is not of class" %<<%
                      dQuote("numeric") %<<<% '.')
    expect_true(all_are(list(1L, 2L), 'integer'))
}

