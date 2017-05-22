

`%\\%` <- paste
if(FALSE){#! @testing backslash
    a <- 'A vain chalks above the integrated biscuit. '
    b <- 'Within the ground burns the leader.'
    c <- 'How can the fifteen distress lose?'
    expect_equal(a %\% b, paste(a,b))
    expect_equal(a %\% b %\% c, paste(a,b,c))
}


`%\\n%` <- function(x,y){paste(x,y, sep='\n')}
if(FALSE){#! @testing newline-concatenation
    a <- 'A vain chalks above the integrated biscuit. '
    b <- ' Within the ground burns the leader.'
    c <- 'How can the fifteen distress lose?'
    expect_equal(a %\n% b, paste(a,b, sep='\n'))
    expect_equal(a %\n% b %\n% c, paste(a,b, c, sep='\n'))
}


