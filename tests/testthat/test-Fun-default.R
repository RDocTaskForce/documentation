#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Fun-default.R`')
#line 38 "/rdtf/documentation/R/Fun-default.R"
test_that('default_', {#! @testing
    o <-list( 'documentation::default_test_function::test.arg' = 1
            , 'documentation::inherited.arg' = 2
            , 'default_test_function::fun.global.arg' = 3
            , 'global.arg' = "abc"
            )

    withr::with_options(o, {
        expect_equal(default_('test.arg'      , TRUE, fun='default_test_function', pkg='documentation'), 1    )
        expect_equal(default_('inherited.arg' , TRUE, fun='default_test_function', pkg='documentation'), 2    )
        expect_equal(default_('fun.global.arg', TRUE, fun='default_test_function', pkg='documentation'), 3    )
        expect_equal(default_('global.arg'    , TRUE, fun='default_test_function', pkg='documentation'), 'abc')
        expect_true (default_('no.arg'        , TRUE, fun='default_test_function', pkg='documentation')       )
    })

    withr::with_options(list( 'default_test_function::test.arg' = 1
                            , 'inherited.arg' = 2
                            ), {
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
    })

})
#line 89 "/rdtf/documentation/R/Fun-default.R"
test_that('default', {#! @testing
    opar <- options()
    options( 'documentation::default_test_function::test.arg' = 1
           , 'documentation::inherited.arg'                   = 2
           , 'default_test_function::fun.global.arg'          = 3
           , 'global.arg' = "abc"
           )
    expect_equal(default(test.arg      , TRUE, fun='default_test_function', pkg='documentation'), 1)
    expect_equal(default(inherited.arg , TRUE, fun='default_test_function', pkg='documentation'), 2)
    expect_equal(default(fun.global.arg, TRUE, fun='default_test_function', pkg='documentation'), 3)
    expect_equal(default(global.arg    , TRUE, fun='default_test_function', pkg='documentation'), 'abc')
    expect_true (default(no.arg        , TRUE, fun='default_test_function', pkg='documentation'))


    options( 'default_test_function::test.arg' = 1
           , 'inherited.arg' = 2
           , 'global.arg' = "abc"
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

    expect_equal(default_test_function('test'), 1)
    expect_equal(default_test_function('inherited'), 2)
    expect_equal(default_test_function('global'), 'abc')
    expect_equal(default_test_function('no'), FALSE)
    options(opar)
})
