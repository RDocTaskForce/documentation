#' @include setup.R

`%||%` <- function (x, y) if (is.null(x)) y else x
if(FALSE){#@testing
    expect_true( NULL %||% TRUE)
    expect_true( TRUE %||% FALSE)
}
