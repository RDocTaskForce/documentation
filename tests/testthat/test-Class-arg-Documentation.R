#! This file was automatically produced by documentation::extract_tests on  2018-05-23 19:03:27
#! changes will be overwritten.
context('tests extracted from file `Class-arg-Documentation.R`')
#line 54 "/rdtf/documentation/R/Class-arg-Documentation.R"
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
})
