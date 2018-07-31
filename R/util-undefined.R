



.undefined <- as.name("<UNDEFINED>")
.is_undefined <- function(x)isTRUE(identical(x, .undefined))
if(FALSE){#@testing
    expect_true(.is_undefined(.undefined))
    expect_false(.is_undefined(as.name("my_name")))
}
