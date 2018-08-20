#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Fun-documentation.R`')
#line 28 "R/Fun-documentation.R"
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
#line 77 "R/Fun-documentation.R"
test_that('documentation<-,ANY,Documentation-method', {#! @testing
    x <- 1
    y <- new('BaseDocumentation', title='testing')

    documentation(x) <- y
    expect_identical(attr(x, 'documentation'), y)
    expect_identical(documentation(x), y)

    expect_error( documentation(x) <- 'this should not work'
                , class = 'documentation-error-invalid'
                )
    expect_message( documentation(x) <- y
                  , class = 'documentation-message-overwrite'
                  )
})
