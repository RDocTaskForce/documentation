
#' Alias for gettext/gettextf
._ <- function(msg, ..., domain=NULL){
    if (...length())
        gettextf(msg, ..., domain = domain)
    else
        gettext(msg, domain = domain)
}

#' alias for structure
s <- structure

#' Equivalent to add_class
cl <- function(x, new)s(x, class=c(new, class(s)))

#' Shortcut for creating text vectors without quotes.
.T <- function (...) #< names and literal strings.
{
    c <- as.character(x <- substitute(c(...)))[-1]
    names(c) <- names(as.list(x))[-1]
    return(c)
    #' @return a character vector, optionally with names.
}

clean_Rd <- tools:::toRd.default

