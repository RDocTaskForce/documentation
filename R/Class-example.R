#' @include utils.R

setClass("Documentation-example", contains='VIRTUAL')

setClass("example", contains=c("Documentation-example", "expression"))
if(FALSE){#@testing
    simple.text <- "
    # prints hello world.
    hw()
    "
    p <- parse(text=simple.text, keep.source=TRUE)
    ex <- new('example', p)

    expect_is(ex, 'Documentation-example')
    expect_equal(getSrcref(ex), getSrcref(p))



    ex2 <- new('example', expression(test(x,y)))
    expect_is(ex2, 'Documentation-example')
    expect_null(getSrcref(ex2))
}

setVector('example', 'Documentation-Examples')
if(FALSE){#@tesing
    ex <- new("example")
    expect_identical( as(ex, 'Documentation-Examples')
                    , new('Documentation-Examples', list(ex)))
}

setAs('character', 'example', function(from){
    p <- parse(text=from, keep.source = TRUE)
    new('example', p)
})
if(FALSE){#@testing
    txt <- "# example given as text" %\%
           "test(x,y)"
    ex <- as(txt, 'example')
    expect_equal( S3Part(ex, TRUE)[[1]]
                , expression(test(x,y))[[1]]
                , check.attributes=FALSE)
}
