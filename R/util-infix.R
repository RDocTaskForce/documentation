
#' @rdname documentation-infix-operators
#' @title Infix string concatenation.
#' 
#' @param lhs left string
#' @param rhs right string
#' 
#' @details
#' The infix operators listed here are three versions of paste.
#' \itemize{
#'   \item \code{\%\\\%} is for preserving line breaks
#'   \item \code{\%<<\%} is an infix replacement for \code{\link{paste}}
#'   \item \code{\%<<<\%} is paste with no space and no break."
#' }
#' @aliases %\\%
#' @export %\% %<<% %<<<%
`%<<%` <- function(lhs, rhs){
    if (is.null(rhs)) return(collapse(lhs))
    else if (is.null(lhs)) return(collapse(rhs))
    else return(paste(collapse(lhs), collapse(rhs), sep=" "))
}
if(FALSE){#! @testing %<<%
    a <- 'A vain chalks above the integrated biscuit. '
    b <- 'Within the ground burns the leader.'
    c <- 'How can the fifteen distress lose?'
    expect_equal(a %<<% b, paste(a,b))
    expect_equal(a %<<% b %<<% c, paste(a,b,c))
    
    expect_equal(a %<<% NULL, a)
    expect_equal(NULL %<<% a, a)
    expect_equal(NULL %<<% NULL, "")
}

#' @rdname documentation-infix-operators
`%<<<%` <- function(lhs, rhs) paste(collapse0(lhs), collapse(rhs), sep="")

if(FALSE){#! @testing %<<<%
    a <- 'A vain chalks above the integrated biscuit. '
    b <- ' Within the ground burns the leader.'
    c <- 'How can the fifteen distress lose?'
    expect_equal(a %<<<% b, paste0(a,b))
    expect_equal(a %<<<% b %<<<% c, paste0(a,b, c, sep=''))
    
    expect_equal(a %<<<% NULL, a)
    expect_equal(NULL %<<<% a, a)
    expect_equal(NULL %<<<% NULL, '')
}

`%\\%` <- function(lhs, rhs) paste(collapse(lhs, '\n'), collapse(rhs, '\n'), sep="\n")
if(FALSE){#! @testing newline-concatenation
    a <- 'A vain chalks above the integrated biscuit. '
    b <- ' Within the ground burns the leader.'
    c <- 'How can the fifteen distress lose?'
    expect_equal(a %\% b, paste(a,b, sep='\n'))
    expect_equal(a %\% b %\% c, paste(a,b, c, sep='\n'))
}

`%||%` <- function (x, y) if (is.null(x)) y else x
if(FALSE){#@testing
    expect_true( NULL %||% TRUE)
    expect_true( TRUE %||% FALSE)
}