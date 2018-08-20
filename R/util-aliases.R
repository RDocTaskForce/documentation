
#' Alias for gettext/gettextf
._ <- function(msg, ..., domain=NULL){
    if (...length())
        gettextf(msg, ..., domain = domain)
    else
        gettext(msg, domain = domain)
}

#' alias for structure
#'
#' @param .Data An object, any object.
#' @param ...   Attributes, names will be inferred from
#'              passed objects if not named
s <- function( .Data, ...){
    new.attr <- list(...)
    if (is.null(names(new.attr)))
        names(new.attr) <- as.character(substitute(c(...)))[-1]
    else if(any(. <- is.na(names(new.attr)) | names(new.attr) == ''))
        names(new.attr) <- ifelse(., as.character(substitute(c(...)))[-1], names(new.attr))

    for (a in names(new.attr))
        attr(.Data, a) <- new.attr[[a]]
    return(.Data)
}
if(FALSE){#@testing
    msg <- "An failure message"
    val <-s(FALSE, msg)
    expect_identical(attributes(val), list(msg=msg))


    val <- s(FALSE, msg, count = 5)
    expect_identical(attributes(val), list(msg=msg, count=5))

    val <- s(c(a=1, b=2), count=2)
    expect_identical(names(val), c('a','b'))
}

#' Equivalent to add_class
#'
#' @param x an object, but not an S4 object
#' @param new the new class to append.
cl <- function(x, new){s(x, class=c(new, attr(x, 'class')))}
if(FALSE){#@testing
    x <- cl(TRUE, 'success')
    expect_is(x, 'success')

    y <- cl(x, 'a big success')
    expect_is(y, 'success')
    expect_is(y, 'a big success')

    expect_identical(cl('text', 'class')
                    , structure('text', class='class'))
}



#' Shortcut for creating text vectors without quotes.
.T <- function(...) #< names and literal strings.
{
    x <- substitute(c(...))
    val <- as.character(x)
    if (!is.null(n <- names(x)))
        names(val) <- ifelse( is.na(n) | n == ''
                            , as.character(x)
                            , names(x))
    return(val[-1L])
    #' @return a character vector, optionally with names.
}
if(FALSE){#@testing
    expect_equal(.T(._, s, cl, .T)
                , c('._', 's', 'cl', '.T')
                )
    expect_equal( .T(a=._, s, cl, .T)
                , c(a='._', s='s', cl='cl', .T='.T')
                )
}




clean_Rd <- tools:::toRd.default

