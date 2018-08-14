
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
    val <- (x <- 'it still evaluated') %unless% TRUE
    expect_null(val)
    expect_false(exists('x'))

    val <- (x <- 'it still evaluated') %unless% TRUE %then% "should get this"
    expect_equal(val, "should get this")
    expect_false(exists('x'))

    val <- (x <- 'it is supposed to be evaluated') %unless% FALSE %then% "should not get this"
    expect_equal(val, 'it is supposed to be evaluated')
    expect_true(exists('x'))
    expect_equal(x, 'it is supposed to be evaluated')

    expect_error( 'this' %if% 'wont' %then% 'work'
                , "Infix opperator '%then%' can only be used" %<<%
                  "following an '%unless%' infix."
                )
}

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

    val <- (x <- 'it still evaluated') %if% FALSE %otherwise% "should get this"
    expect_equal(val, "should get this")
    expect_false(exists('x'))

    val <- (x <- 'it is supposed to be evaluated') %if% TRUE %otherwise% "should not get this"
    expect_equal(val, 'it is supposed to be evaluated')
    expect_true(exists('x'))
    expect_equal(x, 'it is supposed to be evaluated')

    expect_error( 'this' %unless% 'wont' %otherwise% 'work'
                , "Infix opperator '%otherwise%' can only be used" %<<%
                  "following an '%if%' infix."
                )
}




