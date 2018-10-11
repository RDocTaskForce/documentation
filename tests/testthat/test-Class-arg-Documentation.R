#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Class-arg-Documentation.R`')
#line 61 "/rdtf/documentation/R/Class-arg-Documentation.R"
test_that('arg', {#! @testing arg
    a <- new( "arg-Documentation"
            , name= 'testing'
            , description='a testing argument'
            , default=new('Documentation-No-Default-Value')
            )
    expect_identical(a@name       , as.name('testing'))
    expect_identical(a@description, 'a testing argument')
    expect_identical(a@default    , new('Documentation-No-Default-Value'))

    b <- arg_('testing', 'a testing argument')
    expect_identical(a,b)

    c <- arg(testing, 'a testing argument')
    expect_identical(a,c)


    d <- arg(testing, NA_character_, NULL)
    expect_identical(d@default, new('Documentation-Default-Value:NULL'))
    expect_identical(d@description, NA_character_)

    e <- arg(testing, NA, NULL)
    expect_identical(e@description, NA_character_)

    testing <- "SAY MY NAME!"
    b <- arg(name= testing, description='a testing argument')
    expect_identical(a,b)

    L <- new("ArgumentList")
    L[[1]] <- a
    L[[2]] <- b

    expect_is(L, 'ArgumentList')
    expect_equal(length(L), 2)

    M <- ArgumentList(a, b)
    expect_identical(L, M)

    expect_identical(c(M, c), AL(a,b,c))
    expect_identical(c(M, AL(c,d)), AL(a,b,c,d))
})
#line 102 "/rdtf/documentation/R/Class-arg-Documentation.R"
test_that('unique.ArgumentList and [.ArgumentList', {#@testing unique.ArgumentList and [.ArgumentList
    a <- arg(a, 'first')
    b <- arg(b, 'second')
    c <- arg(c, 'last')

    x <- AL(a,b)

    expect_identical(y <- c(x, a, b, c), AL(a, b, a, b, c))
    expect_identical(unique(y), AL(a,b,c))

    expect_identical(y[c(1,3)], AL(a,a))
})
#line 117 "/rdtf/documentation/R/Class-arg-Documentation.R"
test_that('`c.arg-Documentation`', {#@testing
    a <- arg(a, 'first')
    b <- arg(b, 'second')

    x <- c(a,b)
    expect_identical(x, AL(a,b))
})
