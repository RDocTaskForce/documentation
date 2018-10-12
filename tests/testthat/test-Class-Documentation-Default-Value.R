#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Class-Documentation-Default-Value.R`')
#line 46 "R/Class-Documentation-Default-Value.R"
test_that('Documentation-Default-Value', {#@testing Documentation-Default-Value
    expect_is(dv.lgl <- new('Documentation-Default-Value:logical'   , TRUE             ), 'Documentation-Default-Value:logical'   )
    expect_is(dv.dbl <- new('Documentation-Default-Value:numeric'   , 2.5              ), 'Documentation-Default-Value:numeric'   )
    expect_is(dv.lst <- new('Documentation-Default-Value:list'      , list(1,2,3)      ), 'Documentation-Default-Value:list'      )
    expect_is(dv.exp <- new('Documentation-Default-Value:expression', expression()     ), 'Documentation-Default-Value:expression')
    expect_is(dv.chr <- new('Documentation-Default-Value:character' , ""               ), 'Documentation-Default-Value:character' )
    expect_is(dv.fun <- new('Documentation-Default-Value:function'  , function(){}     ), 'Documentation-Default-Value:function'  )
    expect_is(dv.nam <- new('Documentation-Default-Value:name'      , as.name('test')  ), 'Documentation-Default-Value:name'      )
    expect_is(dv.nul <- new('Documentation-Default-Value:NULL'                         ), 'Documentation-Default-Value:NULL'      )
    expect_is(          new('Documentation-Default-Value:NULL'      , NULL             ), 'Documentation-Default-Value:NULL'      )

    expect_is( as(as.name('hello'), 'Documentation-Default-Value'), 'Documentation-Default-Value')
    expect_is( as(substitute(x+y) , 'Documentation-Default-Value'), 'Documentation-Default-Value')

    expect_error( dv <- new('Documentation-Default-Value')
                , "trying to generate an object from a virtual class"
                )

    expect_identical(as.character(.no.default), '')

    expect_identical( as(TRUE             , 'Documentation-Default-Value'), dv.lgl)
    expect_identical( as(2.5              , 'Documentation-Default-Value'), dv.dbl)
    expect_identical( as(list(1,2,3)      , 'Documentation-Default-Value'), dv.lst)
    expect_identical( as(expression()     , 'Documentation-Default-Value'), dv.exp)
    expect_identical( as(""               , 'Documentation-Default-Value'), dv.chr)
    expect_identical( as(function(){}     , 'Documentation-Default-Value'), dv.fun)
    expect_identical( as(as.name('test')  , 'Documentation-Default-Value'), dv.nam)
    expect_identical( as(NULL             , 'Documentation-Default-Value'), dv.nul)
})
