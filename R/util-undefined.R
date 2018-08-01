


#' Check if an name is undefined
#'
#' @param x object to check, should be a name from a documentation object slot.
.is_undefined <- function(x)isTRUE(identical(x, .undefined))

#' @rdname ._is_undefined
.undefined <- as.name("<UNDEFINED>")
if(FALSE){#@testing
    expect_true(.is_undefined(.undefined))
    expect_false(.is_undefined(as.name("my_name")))
}
