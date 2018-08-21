#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Fun-extract_documentation.R`')
#line 62 "R/Fun-extract_documentation.R"
test_that('.get_roxy_block', {#@testing
    test.file <- system.file("examples", "example_character.R", package='documentation')
    sys.source( test.file, keep.source=TRUE)

    expect_error(.get_roxy_block(example_character), class='documentation-error-no_src')
    block <- .get_roxy_block(example_character, srcfile=test.file)

    expect_is(block, 'roxy_block')
    expect_equal(block$title, "An example character vector")
    expect_equal(attr(block, 'object')$alias, 'example_character')
})
#line 80 "R/Fun-extract_documentation.R"
test_that('get_parse_data.roxy_block', {#@testing
    test.file <- system.file("examples", "example_function1.R", package='documentation')
    sys.source( test.file, keep.source=TRUE)
    expect_true(exists("example_function1"))
    pd <- get_parse_data(example_function1)

    roxy.block <- .get_roxy_block(example_function1, srcfile=test.file)

    val <- get_parse_data(roxy.block)
    expect_is(val, 'parse-data')
    expect_equal( as.data.frame(val)
                , as.data.frame(pd)
                , check.attributes=FALSE)
    expect_equal( attr(val, 'root'), attr(pd, 'root') )
    expect_false( attr(val, 'id') == attr(pd, 'id') )
})
#line 157 "R/Fun-extract_documentation.R"
test_that('.construct_documentation.function', {#@testing
    test.file <- system.file("examples", "example_function1.R", package='documentation')
    sys.source( test.file, keep.source=TRUE)
    expect_true(exists("example_function1"))
    pd <- get_parse_data(example_function1)

    roxy.block <- .get_roxy_block(example_function1, srcfile=test.file)

    docs <- .construct_documentation.function(example_function1, roxy.block, pd)
    expect_is(docs, 'function-Documentation')

    docs2 <- .construct_documentation.function(example_function1, roxy.block)
    expect_is(docs2, 'function-Documentation')
    expect_identical(docs, docs2)

    attr(pd, 'id') <- attr(pd, 'root') <- NULL
    docs3 <- .construct_documentation.function(example_function1, roxy.block, pd)
    expect_is(docs3, 'function-Documentation')
    expect_identical(docs, docs3)


})
#line 179 "R/Fun-extract_documentation.R"
test_that('.construct_documentation.function', {#@testing
    test.file <- system.file("examples", "example_multiple.R", package='documentation')
    sys.source( test.file, keep.source=TRUE)
    expect_true(exists("example_function1"))
    pd <- get_parse_data(parse(file=test.file, keep.source=TRUE))
    expect_length(pd_all_root_ids(pd), 3)

    expect_error( .construct_documentation.function(example_function1, roxy.block, pd)
                , class='documentation-error-bad_pd')
})
#line 189 "R/Fun-extract_documentation.R"
test_that('.construct_documentation.function', {#@testing
    text <- deparse(.construct_documentation.function)
    pd <- get_parse_data(parse(text=text, keep.source=TRUE))

    expect_error(.construct_documentation.function( .construct_documentation.function, NULL, pd)
                , class='documentation-error-no_comments')
})
#line 196 "R/Fun-extract_documentation.R"
test_that('.construct_documentation.function', {#@testing
    text <- "
    #' Testing for bad arguments
    #'
    #' A description
    #'
    #' @param greeting a message
    #' @param who whom to greet
    hw <- function( greeting = 'hello' #< What to say.
                  , who = 'world'      #< who to say it to.
                  ){
        cat(greeting, who)
    }
    "
    pd <- get_parse_data(p<- parse(text=text, keep.source=TRUE))
    eval(p)

    roxy <- roxygen2::parse_text(text)[[1]]
    expect_is(roxy, 'roxy_block')
    expect_error(.construct_documentation.function( p, roxy , pd)
                , class='documentation-error')
})
#line 250 "R/Fun-extract_documentation.R"
test_that('extract_documentation', {#@testing
    test.file <- system.file("examples", "example_multiple.R", package='documentation')
    sys.source( test.file, keep.source=TRUE)
    expect_true(exists("example_function1"))
    pd <- get_parse_data(parse(file=test.file, keep.source=TRUE))
    expect_length(pd_all_root_ids(pd), 3)

    roxys <- roxygen2::parse_file(test.file)

    expect_error( extract_documentation(example_function1, pd=iris)
                , class='documentation-error-invalid_argument')
    expect_error( extract_documentation(example_function1, pd=pd)
                , class='documentation-error-bad_pd')

    expect_error(extract_documentation(example_function1, roxy.block = list())
                , class='documentation-error-invalid_argument')
    expect_error(extract_documentation(example_function1, roxy.block = roxys[[3]])
                , class='documentation-error-bad_roxy')

    docs <- extract_documentation(example_function1)
})
#line 311 "R/Fun-extract_documentation.R"
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
#line 332 "R/Fun-extract_documentation.R"
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
#line 346 "R/Fun-extract_documentation.R"
test_that('extract_documentation.function errors', {#@testing extract_documentation.function errors
    test.file <- system.file("examples", "example_multiple.R", package='documentation')
    sys.source( test.file, keep.source=TRUE)
    expect_true(exists("example_function1"))
    pd <- get_parse_data(parse(file=test.file, keep.source=TRUE))
    expect_length(pd_all_root_ids(pd), 3)
    roxys <- roxygen2::parse_file(test.file)

    expect_error( extract_documentation(example_function1, pd=iris)
                , class='documentation-error-invalid_argument')
    expect_error( extract_documentation(example_function1, pd=pd)
                , class='documentation-error-bad_pd')

    expect_error(extract_documentation(example_function1, roxy.block = list())
                , class='documentation-error-invalid_argument')
    expect_error(extract_documentation(example_function1, roxy.block = roxys[[3]])
                , class='documentation-error-bad_roxy')

    expect_warning( extract_documentation.function(example_function1)
                  , "method should not be called directly" )

    docs <- extract_documentation(example_function1)

    spec.pd <- get_parse_data(example_function1)
    attr(spec.pd, 'id') <- attr(spec.pd, 'root') <- NULL

    docs2 <- extract_documentation(example_function1, pd=spec.pd)
    expect_identical(docs2, docs)
})
#line 375 "R/Fun-extract_documentation.R"
test_that('extract_documentation.function', {#@testing
    text <- "
    #' Testing name mismatch
    #'
    #' A description
    #'
    #' @name hello_world
    #'
    hw <- function( greeting = 'hello' #< What to say.
                  , who = 'world'      #< who to say it to.
                  ){
        cat(greeting, who)
    }
    "
    txt.pd <- get_parse_data(p<- parse(text=text, keep.source=TRUE))
    eval(p)

    txt.roxy <- roxygen2::parse_text(text)[[1]]
    attr(txt.roxy, 'object')$alias <-NULL

    expect_error(extract_documentation( hw
                                      , pd=txt.pd
                                      , roxy.block = txt.roxy
                                      , name = 'hw')
                , class="documentation-error")

})
#line 402 "R/Fun-extract_documentation.R"
test_that('extract_documentation.function no_doc_comments.', {#@testing extract_documentation.function no_doc_comments.
    text <- "
    hw <- function( greeting = 'hello'
                  , who = 'world'
                  ){
        cat(greeting, who)
    }
    "
    txt.pd <- get_parse_data(p<- parse(text=text, keep.source=TRUE))
    eval(p)

    expect_message( docs <- extract_documentation( hw )
                  , class="documentation-message-no_doc_comments")

    expect_identical(doc_get_name(docs), 'hw')
})
#line 453 "R/Fun-extract_documentation.R"
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

    expect_warning(docs2 <- with(env, extract_documentation.standardGeneric(example_generic)))

    expect_identical(docs2, docs)
})
#line 474 "R/Fun-extract_documentation.R"
test_that('extract_documentation.standardGeneric no_src error', {#@testing extract_documentation.standardGeneric no_src error
    env <- new.env()
    env$.packageName <- "documentation-testing-environment"

    test.file <- system.file("examples", "standardGeneric.R", package = "documentation")
    sys.source( test.file, envir = env , keep.source = FALSE)

    env$pd <- get_parse_data(parse(file=test.file, keep.source=TRUE))

    expect_null(attr(env$example_generic@default, 'srcref'))

    docs <- with(env, {
        expect_error( extract_documentation(example_generic, pd=pd)
                    , class="documentation-error-no_src")
    })

})
