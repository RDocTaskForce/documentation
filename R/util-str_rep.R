collapse0 <- function(x, with=''){paste(x, collapse=with)}
collapse  <- function(x, with=' '){paste(x, collapse=with)}
collapse_nl<- function(x, with='\n'){paste(x, collapse=with)}
str_rep <- function(x, len, sep=''){paste(rep_len(x, length.out = len), collapse=sep)}

if(FALSE){#@testing
    expect_equal(str_rep('#', 3), '###')
    expect_equal(str_rep(c('r', 'l'), 5), "rlrlr")
}


