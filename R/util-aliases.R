
#' Alias for gettext/gettextf
._ <- function(msg, ..., domain=NULL){
    if (...length())
        gettextf(msg, ..., domain = domain)
    else
        gettext(msg, domain = domain)
}

#' alias for structure
s <- function(x, ...){
    new.attr <- list(...)
    if (is.null(names(new.attr)))
        names(new.attr) <- as.character(substitute(c(...)))[-1]
    else if(any(. <- is.na(names(new.attr)) | names(new.attr) == ''))
        names(new.attr) <- ifelse(., as.character(substitute(c(...)))[-1], names(new.attr))
    attributes(x) <- new.attr
    return(x)
}
if(FALSE){#@testing
    msg <- "An failure message"
    val <- s(FALSE, msg, count = 5)
    expect_identical(attributes(val), list(msg=msg, count=5))
    
}



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

