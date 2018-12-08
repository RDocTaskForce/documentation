collapse_nl<- function(x, with='\n'){paste(x, collapse=with)}
str_rep <- function(x, len, sep=''){paste(rep_len(x, length.out = len), collapse=sep)}

if(FALSE){#@testing
    expect_equal(str_rep('#', 3), '###')
    expect_equal(str_rep(c('r', 'l'), 5), "rlrlr")
}


space <- function(n=1)str_rep(' ', n)
if(FALSE){#@testing
    expect_identical(space(), ' ')
    expect_identical(space(0), '')
    expect_identical(space(3), '   ')
}

is_whitespace <- function(x){
    grepl("^\\s+$", x)
}
if(FALSE){#@testing
    expect_true(is_whitespace(" "))
    expect_true(is_whitespace("\t"))
    expect_false(is_whitespace("t"))
    expect_false(is_whitespace(""))
}
