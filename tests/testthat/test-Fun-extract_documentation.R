#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Fun-extract_documentation.R`')
#line 62 "/rdtf/documentation/R/Fun-extract_documentation.R"
test_that('.get_roxy_block', {#@testing
    test.file <- system.file("examples", "example_character.R", package='documentation')
    sys.source( test.file, keep.source=TRUE)

    expect_error(.get_roxy_block(example_character), class='documentation-error-no_src')
    block <- .get_roxy_block(example_character, srcfile=test.file)

    expect_is(block, 'roxy_block')
    expect_equal(block$title, "An example character vector")
    expect_equal(attr(block, 'object')$alias, 'example_character')
})
#line 137 "/rdtf/documentation/R/Fun-extract_documentation.R"
test_that('.construct_documentation.function', {#@testing
    test.file <- system.file("examples", "example_function1.R", package='documentation')
    sys.source( test.file, keep.source=TRUE)
    expect_true(exists("example_function1"))
    pd <- get_parse_data(example_function1)

    roxy.block <- .get_roxy_block(example_function1, srcfile=test.file)

    docs <- .construct_documentation.function(example_function1, roxy.block, pd)
    expect_is(docs, 'function-Documentation')
})
#line 245 "/rdtf/documentation/R/Fun-extract_documentation.R"
test_that('extract_documentation.function with example_function1', {#@testing extract_documentation.function with example_function1
    test.file <- system.file("examples", "example_function1.R", package='documentation')
    sys.source( test.file, keep.source=TRUE)
    expect_true(exists("example_function1"))
    pd <- get_parse_data(example_function1)

    object <- example_function1

    docs <- extract_documentation(example_function1)

    expect_is(docs, 'function-Documentation')
    expect_equal( docs@arguments$x
                , arg(x, "inline documentation for x")
                )
    expect_equal( names(docs@arguments)
                , names(formals(example_function1))
                )

    expect_error( documentation(example_function1)
                , class = 'documentation-error-dnf')
})
#line 266 "/rdtf/documentation/R/Fun-extract_documentation.R"
test_that('extract_documentation.function with example_function2', {#@testing extract_documentation.function with example_function2
    test.file <- system.file("examples", "example_function2.R", package='documentation')
    sys.source( test.file, environment(), keep.source=TRUE)
    expect_true(exists("example_function2"))

    expect_warning( docs <- extract_documentation(example_function2)
                  , class="documentation-warning-orphan_comment"
                  )

    expect_is(docs, 'function-Documentation')
    expect_identical(docs@arguments$x@description, "The x argument description.")
    expect_identical(docs@arguments$y@description, "The y argument description takes 2 lines.")
    expect_identical(docs@name, as.name("example_function2"))
})
#line 314 "/rdtf/documentation/R/Fun-extract_documentation.R"
test_that('with example_generic', {#@testing with example_generic
    env <- new.env()
    env$.packageName <- "documentation-testing-environment"

    test.file <- system.file("examples", "standardGeneric.R", package = "documentation")
    sys.source( test.file, envir = env , keep.source = TRUE)
    docs <- with(env, extract_documentation(example_generic))

    expect_identical(docs@name, as.name("example_generic"))
    expect_identical(docs@title, "Example Generic Function")
    expect_identical(docs@description
                    , "This is an example of an S4 standardGeneric " %\%
                      "documentation.  This is to be used for testing" %\%
                      "of the functions in the documentation package."  %>%
                        FormattedText())
    expect_identical(docs@value, FormattedText("Methods are restricted to returning an object of class logical."))
})
