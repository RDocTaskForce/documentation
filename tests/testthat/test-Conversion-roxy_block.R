#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Conversion-roxy_block.R`')
#line 76 "R/Conversion-roxy_block.R"
test_that('setAs,roxy_block,function-Documentation', {#@testing setAs,roxy_block,function-Documentation
    test.file <- system.file("examples", "example_function1.R", package='documentation')

    rd_blocks <- roxygen2::parse_file(test.file)
    from <- rd_blocks[[1]]

    expect_is(from, 'roxy_block')

    docs <- as(from, 'Documentation')
    expect_is(docs, 'Documentation')
    expect_is(docs, 'function-Documentation')
    expect_equal(docs@name, as.name("example_function1"))
    expect_equal(docs@title, "This is the title")
    expect_equal(docs@description, FormattedText("This is the description block.\nIt takes multiple lines."))
    expect_length(docs@arguments,1)
    expect_null(docs@arguments$x)
    expect_equal(docs@arguments$y, arg(y, "explicit documentation for y"))
})
#line 94 "R/Conversion-roxy_block.R"
test_that('as(roxy_block, "function-Documentation")', {#@testing
    text <- "
    #' Testing name mismatch
    #'
    #' A description
    #'
    #' @name hello_world
    #' @aliases example_hello_world
    hw <- function( greeting = 'hello' #< What to say.
                  , who = 'world'      #< who to say it to.
                  ){
        cat(greeting, who)
    }
    "
    txt.roxy <- roxygen2::parse_text(text)[[1]]
    expect_warning( doc <- as(txt.roxy, 'function-Documentation')
                  , class="documentation-warning-roxy_block")

    expect_equal( doc_get_name(doc), "hello_world")
    expect_equal( doc_get_aliases(doc), c("example_hello_world", "hw"))
})
