
#' Alias for gettext/gettextf
._ <- function(msg, ..., domain=NULL){
    if (...length())
        gettextf(msg, ..., domain = domain)
    else 
        gettext(msg, domain = domain)
}


s <- structure

