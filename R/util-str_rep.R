collapse0 <- function(x, with=''){paste(x, collapse=with)}
collapse  <- function(x, with=' '){paste(x, collapse=with)}
str_rep <- function(x, times){collapse(rep.int(x, times=times))}

if(FALSE){
    expect_equal(str_rep('#', 3), '###')
}


