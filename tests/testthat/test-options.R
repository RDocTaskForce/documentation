#! This file was automatically produced by documentation::extract_tests on  2018-08-03 01:24:29
#! changes will be overwritten.
context('tests extracted from file `options.R`')
#line 32 "/rdtf/documentation/R/options.R"
test_that('default_', {#! @testing
    opar <- options()
    
    options( 'defaults::documentation::default_test_function::test.arg' = 1
           , 'defaults::documentation::inherited.arg' = 2
           , 'defaults::default_test_function::fun.global.arg' = 3
           , 'defaults::global.arg' = "abc"
           )

    expect_equal(default_('test.arg'      , TRUE, fun='default_test_function', pkg='documentation'), 1    )
    expect_equal(default_('inherited.arg' , TRUE, fun='default_test_function', pkg='documentation'), 2    )
    expect_equal(default_('fun.global.arg', TRUE, fun='default_test_function', pkg='documentation'), 3    )
    expect_equal(default_('global.arg'    , TRUE, fun='default_test_function', pkg='documentation'), 'abc')
    expect_true (default_('no.arg'        , TRUE, fun='default_test_function', pkg='documentation')       )
 
    options( 'defaults::default_test_function::test.arg' = 1
           , 'defaults::inherited.arg' = 2
           )

    default_test_function <- 
    function( which = c('test', 'inherited', 'no')
            , test.arg      = default_('test.arg'      , FALSE, pkg=NULL)
            , inherited.arg = default_('inherited.arg' , FALSE, pkg=NULL)
            , no.arg        = default_('no.arg'        , FALSE, pkg=NULL)
            ){
        switch( match.arg(which)
              , test      = test.arg
              , inherited = inherited.arg
              , no        = no.arg
              )
    }
    default_test_function('test')
    
    expect_equal(default_test_function('test'), 1)
    expect_equal(default_test_function('inherited'), 2)
    expect_equal(default_test_function('no'), FALSE)
    
    options(opar)
})
#line 91 "/rdtf/documentation/R/options.R"
test_that('default', {#! @testing
    opar <- options()
    options( 'defaults::documentation::default_test_function::test.arg' = 1
           , 'defaults::documentation::inherited.arg'                   = 2
           , 'defaults::default_test_function::fun.global.arg'          = 3
           , 'defaults::global.arg' = "abc"
           )
    expect_equal(default(test.arg      , TRUE, fun='default_test_function', pkg='documentation'), 1)
    expect_equal(default(inherited.arg , TRUE, fun='default_test_function', pkg='documentation'), 2)
    expect_equal(default(fun.global.arg, TRUE, fun='default_test_function', pkg='documentation'), 3)
    expect_equal(default(global.arg    , TRUE, fun='default_test_function', pkg='documentation'), 'abc')
    expect_true (default(no.arg        , TRUE, fun='default_test_function', pkg='documentation'))


    options( 'defaults::default_test_function::test.arg' = 1
           , 'defaults::inherited.arg' = 2
           , 'defaults::global.arg' = "abc"
           )

    default_test_function <- 
    function( which = c('test', 'inherited', 'global', 'no')
            , test.arg      = default(test.arg      , FALSE, pkg=NULL)
            , inherited.arg = default(inherited.arg , FALSE, pkg=NULL)
            , global.arg    = default(global.arg    , FALSE, pkg=NULL)    
            , no.arg        = default(no.arg        , FALSE, pkg=NULL)
            ){
        switch( match.arg(which)
              , test      = test.arg
              , inherited = inherited.arg
              , global    = global.arg
              , no        = no.arg
              )
    }
    default_test_function('global')
    
    expect_equal(default_test_function('test'), 1)
    expect_equal(default_test_function('inherited'), 2)
    expect_equal(default_test_function('global'), 'abc')
    expect_equal(default_test_function('no'), FALSE)
    options(opar)
})
