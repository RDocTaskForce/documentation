#! This file was automatically produced by documentation::extract_tests on  2018-08-03 01:24:28
#! changes will be overwritten.
context('tests extracted from file `Class-Documentation-Default-Value.R`')
#line 41 "/rdtf/documentation/R/Class-Documentation-Default-Value.R"
test_that('Documentation-Default-Value', {#!@testing Documentation-Default-Value
    expect_is(new('Documentation-Default-Value:logical'   , TRUE             ), 'Documentation-Default-Value:logical'   )
    expect_is(new('Documentation-Default-Value:numeric'   , 2.5              ), 'Documentation-Default-Value:numeric'   )
    expect_is(new('Documentation-Default-Value:list'      , list(1,2,3)      ), 'Documentation-Default-Value:list'      )
    expect_is(new('Documentation-Default-Value:expression', expression()     ), 'Documentation-Default-Value:expression')
    expect_is(new('Documentation-Default-Value:character' , ""               ), 'Documentation-Default-Value:character' )
    expect_is(new('Documentation-Default-Value:function'  , function(){}     ), 'Documentation-Default-Value:function'  )
    expect_is(new('Documentation-Default-Value:name'      , as.name('test')  ), 'Documentation-Default-Value:name'      )
    expect_is(new('Documentation-Default-Value:NULL'                         ), 'Documentation-Default-Value:NULL'      )
    expect_is(new('Documentation-Default-Value:NULL'      , NULL             ), 'Documentation-Default-Value:NULL'      )
    
    expect_is( as(as.name('hello'), 'Documentation-Default-Value'), 'Documentation-Default-Value')
    expect_is( as(substitute(x+y) , 'Documentation-Default-Value'), 'Documentation-Default-Value')
})
