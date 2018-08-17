#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Fun-default.R`')
#line 151 "/rdtf/documentation/R/Fun-default.R"
test_that('default_', {#! @testing
    o <-list( 'documentation::default_test_function::test.arg' = 1
            , 'documentation::inherited.arg' = 2
            , 'global.arg' = "abc"
            )
    withr::with_options(o, {
        test_default <- function(name){
            default_(name, TRUE, fun='default_test_function', pkg='documentation')
        }
        expect_equal(test_default('test.arg')     , 1    )
        expect_equal(test_default('inherited.arg'), 2    )
        expect_equal(test_default('global.arg'   ), 'abc')
        expect_true (test_default('no.arg'       )      )
    })

    withr::with_options(list( 'default_test_function::test.arg' = 1
                            , 'inherited.arg' = 2
                            , 'global.arg' = 3
                            ), {
        default_test_function <-
        function( which = c('test', 'inherited', 'global', 'no')
                , test.arg      = default_('test.arg'      , FALSE, pkg=NULL)
                , inherited.arg = default_('inherited.arg' , FALSE, pkg=NULL)
                , global.arg    = default_('global.arg'    , FALSE, pkg=NULL)
                , no.arg        = default_('no.arg'        , FALSE, pkg=NULL)
                ){
            switch( match.arg(which)
                  , test      = test.arg
                  , inherited = inherited.arg
                  , global    = global.arg
                  , no        = no.arg
                  )
        }
        default_test_function('test')

        expect_equal(default_test_function('test'), 1)
        expect_equal(default_test_function('inherited'), 2)
        expect_equal(default_test_function('global'), 3)
        expect_equal(default_test_function('no'), FALSE)
    })

    print.my_class <- function( x
                              , ...
                              , test.arg      = default_('test.arg'      , FALSE
                                                        , prefix="DEFAULTS"
                                                        , pkg='test-documentation'
                                                        , suffix="OVERRIDE"
                                                        )
                              ){
        cat( "test.arg=" %<<<% test.arg %<<<% "\n")
        invisible(x)
    }

    if (interactive() && tracingState()) {
        trace(default_, exit =
                  substitute(doc_error("Documentation debugging\n"
                                      , domains=domains
                                      , type = "trace_test")))
        val <- tryCatch( print(s(list(), class='my_class'))
                       , "documentation-error-trace_test" = function(e){attr(e, 'domains')}
                       )
        expect_is(val, 'character')
        expect_length(val, 5)
        expect_identical( val, c("DEFAULTS", "test-documentation", "print", "my_class", "OVERRIDE"))
        untrace(default_)
    }
})
#line 232 "/rdtf/documentation/R/Fun-default.R"
test_that('default', {#! @testing
    opar <- options()
    options( 'documentation::default_test_function::test.arg' = 1
           , 'documentation::inherited.arg'                   = 2
           , 'global.arg' = "abc"
           )
    test_default1 <- function(x=default(test.arg      , TRUE, fun='default_test_function', pkg='documentation'))x
    test_default2 <- function(x=default(inherited.arg , TRUE, fun='default_test_function', pkg='documentation'))x
    test_default3 <- function(x=default(global.arg    , TRUE, fun='default_test_function', pkg='documentation'))x
    test_default4 <- function(x=default(no.arg        , TRUE, fun='default_test_function', pkg='documentation'))x

    expect_equal(test_default1(), 1)
    expect_equal(test_default2(), 2)
    expect_equal(test_default3(), 'abc')
    expect_true (test_default4())

    options(opar)
})
