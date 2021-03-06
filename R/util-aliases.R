#' @include setup.R

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

named <- function(...){
    . <- substitute(c(...))
    inferred <- as.character(.)[-1L]
    val <- list(...)
    if (is.null(n <- names(val)))
        names(val) <- inferred
    else
        names(val) <- ifelse( is.na(n) | n == '', inferred, n)
    val
}
if(FALSE){#@testing
    a <- 1L
    b <- TRUE

    val <- named(a,b)
    expect_identical(val, list(a=a, b=b))

    val <- named(a,b,c='hello')
    expect_identical(val, list(a=a, b=b, c='hello'))
}


clean_Rd <- tools:::toRd.default

get_attr <- function(x, which, default=NULL, exact=TRUE)
    attr(x, which=which, exact=exact) %||% default
if(FALSE){#@testing
    expect_identical(get_attr(s(list(), test='hello'), 'test'), 'hello')
    expect_null     (get_attr(s(list(), test='hello'), 'test2'))
    expect_identical(get_attr(s(list(), test='hello'), 'test3', 'world'), 'world')
}


fwd <-
forward_attributes <- function(value, object){
    mostattributes(value) <- attributes(object)
    return(value)
}
if(FALSE){#@testing
    a <- s( list(Rd_symb("some"))
          , Rd_tag="\\keyword"
          , class=c("Rd_tag", 'Rd'))
    b <- forward_attributes(list, a)
    expect_identical(attributes(a), attributes(b))

    a <- s( matrix(1:6, 2, 3)
          , class = 'rectangle'
          , another='shape'
          )
    b <- forward_attributes(list(), a)
    expect_true('dim' %in% names(attributes(a)))
    expect_false('dim' %in% names(attributes(b)))
    expect_identical( attributes(a)[names(attributes(b))]
                    , attributes(b)
                    )
}

no_attributes <- function(object){
    mostattributes(object) <- NULL
    return(object)
}

no_src <- function(object){s(object, srcref=NULL, wholeSrcref=NULL, srcfile=NULL)}

regex_escape <- function(x){
    gsub('(\\\\^|\\$|\\.|\\||\\?|\\*|\\+|\\(|\\)|\\[|\\{)', '\\\\\\1', x)
}
if(FALSE){#@testing
    expect_identical(regex_escape('Vector(name)'), 'Vector\\(name\\)')
    expect_identical(regex_escape('my.function'), 'my\\.function')
}

getAs <- function(from, to, where = NULL){
    if (isS4(from) &&.hasSlot(from, 'className'))
        from <- from@className
    if (isS4(to) &&.hasSlot(to, 'className'))
        to <- to@className

    if (is.null(where))
        where <- attr(to, 'package') %||% attr(from, 'package') %||% topenv(parent.frame())
    if (is.character(where))
        where <- asNamespace(where)
    assert_that(is(where, 'environment'))

    getMethod('coerce', c(from, to), where=where)
}
if(FALSE){#@testing
    f <- getAs( getClass('MethodDefinition')
              , getClass("usage/S4method")
              )
    expect_is(f, 'MethodDefinition')
}



