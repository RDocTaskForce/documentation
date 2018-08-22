

interleave <-
function(x, y, simplify=TRUE){
    stopifnot( length(x) == length(y) )
    z <- vector('list', length(x) + length(y))
    for( i in seq_along(z) ){
        z[i] <- if(i %% 2) x[i %/% 2 + i %% 2]
                      else y[i %/% 2         ]
    }
    if (simplify)
        z <- simplify2array(z)
    return(z)
}
if(FALSE){#! @testing
    expect_equal(interleave( 1:3, 4:6 ), c(1,4,2,5,3,6) )
    expect_is(interleave( 1:3, 4:6 ), 'integer' )

    expect_error(interleave(1:3, 4))

    expect_equal(interleave( 1:3, 4:6, FALSE ), list(1,4,2,5,3,6) )
    expect_equal(interleave( 1:3, letters[1:3] ), c(1,'a',2,'b',3,'c') )
}


flatten_lines <-
function(l, add.blank=TRUE){
    if (is.character(l)) return(l)
    assert_that( is.list(l)
               , all(purrr::map_lgl(l, is.character))
               , is.flag(add.blank)
               )
    if (length(l)==1)
        return(l[[1]])
    else if (add.blank)
        unlist(head(interleave(l, as.list(rep('', length(l)))), -1L))
    else
        unlist(l)
}
if(FALSE){#!@testing
    l <- lapply(stringi::stri_rand_lipsum(3L), strwrap, 80L)

    expect_identical(flatten_lines(l[[1]]), l[[1]])
    expect_identical(flatten_lines(l[ 1 ]), l[[1]])
    expect_identical(flatten_lines(l, FALSE), unlist(l))
    expect_true( sum(flatten_lines(l)=='') == length(l) -1L)
}
