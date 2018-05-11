
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
`%<<%` <- function(lhs, rhs)paste(lhs, rhs, sep=" ")
if(FALSE){#! @testing %<<%
    a <- 'A vain chalks above the integrated biscuit. '
    b <- 'Within the ground burns the leader.'
    c <- 'How can the fifteen distress lose?'
    expect_equal(a %<<% b, paste(a,b))
    expect_equal(a %<<% b %<<% c, paste(a,b,c))
}

#' @rdname documentation-infix-operators
`%<<<%` <- function(lhs, rhs)paste(lhs, rhs, sep="")
if(FALSE){#! @testing %<<<%
    a <- 'A vain chalks above the integrated biscuit. '
    b <- ' Within the ground burns the leader.'
    c <- 'How can the fifteen distress lose?'
    expect_equal(a %\% b, paste(a,b, sep='\n'))
    expect_equal(a %\% b %\% c, paste(a,b, c, sep='\n'))
}

`%\\%` <- function(lhs, rhs)paste(lhs, rhs, sep="\n")
if(FALSE){#! @testing newline-concatenation
    a <- 'A vain chalks above the integrated biscuit. '
    b <- ' Within the ground burns the leader.'
    c <- 'How can the fifteen distress lose?'
    expect_equal(a %\% b, paste(a,b, sep='\n'))
    expect_equal(a %\% b %\% c, paste(a,b, c, sep='\n'))
}


