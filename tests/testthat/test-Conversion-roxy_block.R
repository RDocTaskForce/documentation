#! This file was automatically produced by documentation::extract_tests on  2018-05-23 19:03:27
#! changes will be overwritten.
context('tests extracted from file `Conversion-roxy_block.R`')
#line 41 "/rdtf/documentation/R/Conversion-roxy_block.R"
test_that('setAs,roxy_block,function-Documentation', {#@testing setAs,roxy_block,function-Documentation
    test.file <- system.file("examples", "example_function1.R", package='documentation')

    rd_blocks <- roxygen2::parse_file(test.file)
    from <- rd_blocks[[1]]
    
    expect_is(from, 'roxy_block')
    
    docs <- as(from, 'Documentation')
    expect_is(docs, 'Documentation')
    expect_is(docs, 'function-Documentation')
    expect_equal(docs@name, as.name("<UNDEFINED>"))
    expect_equal(docs@title, "This is the title")
    expect_equal(docs@description, FormattedText("This is the description block.\nIt takes multiple lines."))
    expect_length(docs@arguments,1)
    expect_null(docs@arguments$x)
    expect_equal(docs@arguments$y, arg(y, "explicit documentation for y"))
})
