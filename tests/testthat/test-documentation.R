#! This file was automatically produced by documentation::extract_tests on  2018-08-03 01:24:28
#! changes will be overwritten.
context('tests extracted from file `documentation.R`')
#line 28 "/rdtf/documentation/R/documentation.R"
test_that('setGeneric("documentation", ...)', {#! @testing
    test_function <- function(x){
        return(x)
    }
    expect_error( documentation(test_function)
                , class='documentation-error-dnf'
                )

    attr(test_function, "documentation") <- "An invalid documentation"
    expect_error( documentation(test_function)
                , class = 'documentation-error-invalid'
                )
    attr(test_function, "documentation") <-
        function_documentation('test_function'
                              , title="A test function"
                              )
    expect_is(documentation(test_function), 'Documentation')
})
#line 70 "/rdtf/documentation/R/documentation.R"
test_that('documentation<-,ANY,Documentation-method', {#! @testing
    x <- 1
    y <- new('Documentation', title='testing')

    documentation(x) <- y
    expect_identical(attr(x, 'documentation'), y)
    expect_identical(documentation(x), y)

    expect_error( documentation(x) <- 'this should not work'
                , 'Documentation can only be set with objects ' %<<%
                  'of, or inheriting from, the `Documentation` class.'
                )

})
