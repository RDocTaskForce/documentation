#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Fun-documentation.R`')
#line 29 "R/Fun-documentation.R"
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
#line 81 "R/Fun-documentation.R"
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
#line 140 "R/Fun-documentation.R"
test_that('set_documentation_,ANY,Documentation-method', {#@testing
    a <- letters

    docs <- data_documentation(title="Letters big and small", description = "Small letters"
                              , source = bibentry() )

    expect_error(documentation(a))
    set_documentation(a, docs)

    d2 <- docs
    d2@aliases <- 'a'
    d2@name <- as.name('a')
    expect_identical(documentation(a), d2)

    A <- LETTERS
    set_documentation(A, docs)
    d3 <- docs
    d3@aliases <- 'A'
    d3@name <- as.name('A')
    expect_identical(documentation(A), d3)

    expect_message(set_documentation(A, documentation(a)))
    d4 <- d2
    d4@aliases <- c('a', 'A')
    expect_identical(documentation(A), d4)
})
