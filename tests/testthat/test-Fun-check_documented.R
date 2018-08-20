#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Fun-check_documented.R`')
#line 28 "R/Fun-check_documented.R"
test_that('is_documented', {#@testing
    test_function <- function(x){}

    expect_false( is_documented('test_function', environment()))
    expect_equal( attr(is_documented('test_function', environment()), 'reason')
                , "documentation-error-dnf"
                )

    attr(test_function, "documentation") <- "An invalid documentation"
    expect_false( is_documented('test_function', environment()))
    expect_equal( attr(is_documented('test_function', environment()), 'reason')
                , "documentation-error-invalid"
                )
    documentation(test_function) <-
        function_documentation('test_function'
                              , title =
                                  "A function for testing documentation."
                              )
    expect_true(is_documented('test_function', environment(), complete=FALSE))
})
