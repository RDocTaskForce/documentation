#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Fun-document.R`')
#line 256 "R/Fun-document.R"
test_that('setGeneric("document", ...)', {#@testing
    env <- environment()
    sys.source( system.file("examples", "example_function1.R", package='documentation')
              , envir = env, keep.source=TRUE )

    expect_false(is_documented('example_function1', env))
    result <- document(example_function1)

    expect_identical(result, example_function1)
    expect_true(is_documented('example_function1', env, complete=FALSE))

    expect_identical( documentation(example_function1)@title
                    , "This is the title")

    expect_identical( documentation(example_function1)@arguments
                    , ArgumentList( x = arg(x, "inline documentation for x")
                                  , y = arg(y, "explicit documentation for y")
                                  ))
    expect_identical( documentation(example_function1)@value
                    , FormattedText("explicit return."))

    expect_identical( documentation(example_function1)@name
                    , as.name("example_function1"))
})
