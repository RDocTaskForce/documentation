#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Class-example.R`')
#line 6 "R/Class-example.R"
test_that('setClass("example", ...)', {#@testing
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
})
#line 35 "R/Class-example.R"
test_that('as(character, "example")', {#@testing
    txt <- "# example given as text" %\%
           "test(x,y)"
    ex <- as(txt, 'example')
    expect_equal( S3Part(ex, TRUE)[[1]]
                , expression(test(x,y))[[1]]
                , check.attributes=FALSE)
})
