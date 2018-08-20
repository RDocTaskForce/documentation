
#' Infix unless-then logic
#'
#' These give logic that can be used as a qualifying statement that occures after the
#' value statement.
#'
#' @usage
#' prior %unless% proposition
#' prior %unless% proposition %then% alternate
#'
#' @param prior Value to be returned unless proposition returns FALSE.
#' @param proposition The logical statement to condition on.
#' @param alternate When proposition returns true and the %then% is provided
#'                  the alternate value is returned.
#'
#' @family postlogic
#' @export %unless% %then%
`%unless%` <- function( prior, proposition ){
    if (!proposition) return(prior)
}
`%then%` <- function( clause, alternate){
    clause.call <- substitute(clause)
    if (clause.call[[1]] != '%unless%')
        stop("Infix opperator '%then%' can only be used" %<<%
             "following an '%unless%' infix.")

    value <- clause.call[[2]]
    predicate <- clause.call[[3]]

    predicate.value <- eval(predicate, envir = parent.frame())

    if (!predicate.value) eval(value, envir = parent.frame()) else alternate
}
if(FALSE){#@testing unless-then logic
    if (exists('x')) rm(list='x')
    val <- (x <- 'it is supposed to be evaluated') %unless% FALSE
    expect_equal(val, 'it is supposed to be evaluated')

    if (exists('x')) rm(list='x')
    val <- (x <- 'it still evaluated') %unless% TRUE
    expect_null(val)
    expect_false(exists('x'))

    if (exists('x')) rm(list='x')
    val <- (x <- 'it still evaluated') %unless% TRUE %then% "should get this"
    expect_equal(val, "should get this")
    expect_false(exists('x'))

    if (exists('x')) rm(list='x')
    val <- (x <- 'it is supposed to be evaluated') %unless% FALSE %then% "should not get this"
    expect_equal(val, 'it is supposed to be evaluated')
    expect_true(exists('x'))
    expect_equal(x, 'it is supposed to be evaluated')

    if (exists('x')) rm(list='x')
    expect_error( 'this' %if% 'wont' %then% 'work'
                , "Infix opperator '%then%' can only be used" %<<%
                  "following an '%unless%' infix."
                )
}
if(FALSE){#@example
    x <- 4
    x <- sqrt(x) %unless% is.complex(x) %then% "This is too hard :("
    x # 2

    x <- 4i
    x <- sqrt(x) %unless% is.complex(x) %then% "This is too hard :("
    x # This is too hard :(
}

#' Infix if-otherwise logic
#'
#' This construction allows logical statements to be placed after the value to be returned.
#'
#' @usage
#' prior %if% proposition
#' prior %if% proposition %otherwise% alternate
#'
#' @param prior The value to be returned if proposition evaluates to TRUE.
#' @param proposition The logical statement to evaluate
#' @param alternate The value to be returned if proposition evaluates to FALSE.
#'
#' @export %if% %otherwise%
`%if%` <- function( prior, proposition ){
    if (proposition) return(prior)
}
`%otherwise%` <- function( clause, alternate){
    clause.call <- substitute(clause)
    if (clause.call[[1]] != '%if%')
        stop("Infix opperator '%otherwise%' can only be used" %<<%
             "following an '%if%' infix.")

    value <- clause.call[[2]]
    predicate <- clause.call[[3]]

    predicate.value <- eval(predicate, envir = parent.frame())

    if (predicate.value) eval(value, envir = parent.frame()) else alternate
}
if(FALSE){#@testing if-otherwise logic
    if (exists('x')) rm(list='x')
    val <- (x <- 'it still evaluated') %if% FALSE
    expect_null(val)
    expect_false(exists('x'))

    if (exists('x')) rm(list='x')
    val <- (x <- 'it still evaluated') %if% FALSE %otherwise% "should get this"
    expect_equal(val, "should get this")
    expect_false(exists('x'))

    if (exists('x')) rm(list='x')
    val <- (x <- 'it is supposed to be evaluated') %if% TRUE %otherwise% "should not get this"
    expect_equal(val, 'it is supposed to be evaluated')
    expect_true(exists('x'))
    expect_equal(x, 'it is supposed to be evaluated')

    if (exists('x')) rm(list='x')
    expect_error( 'this' %unless% 'wont' %otherwise% 'work'
                , "Infix opperator '%otherwise%' can only be used" %<<%
                  "following an '%if%' infix."
                )
}
if(FALSE){#@example
    x <- 1
    x <- (x+1) %if% is.numeric(x) %otherwise% "Hmm this isn't right O.o"
    x # 2

    x <- 1i
    x <- (x+1) %if% is.numeric(x) %otherwise% "Hmm this isn't right O.o"
    x # Hmm this isn't right
}



