#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Fun-accessors.R`')
#line 76 "/rdtf/documentation/R/Fun-accessors.R"
test_that('generic accessors', {#@testing generic accessors
    if (.document.generated){
        expect_is(doc <- documentation(doc_get_name), 'function-Documentation')
        expect_equal(doc@title, "Documentation accessor for name")
        expect_equal( doc@description
                    , FormattedText("This is an automatically generated function for accessing the" %<<%
                                    "name of a documentation object." %<<%
                                    "Specific methods may override the default behavior, " %<<%
                                    "especially when the name" %<<%
                                    "is expected to conform to a standard" %<<%
                                    "or is generated form other known information."))

        expect_is(doc <- documentation(doc_get_title), 'function-Documentation')
        expect_equal(doc@title, "Documentation accessor for title")
        expect_equal( doc@description
                    , FormattedText("This is an automatically generated function for accessing the" %<<%
                                    "title of a documentation object." %<<%
                                    "Specific methods may override the default behavior, " %<<%
                                    "especially when the title" %<<%
                                    "is expected to conform to a standard" %<<%
                                    "or is generated form other known information."))
    }
})
#line 105 "/rdtf/documentation/R/Fun-accessors.R"
test_that('doc_details<-,Documentation-method', {#@testing
    doc <- function_documentation(name='test-doc')
    det <- FT(stringi::stri_rand_lipsum(3))

    expect_null(doc_get_details(doc))
    doc_details(doc) <- det
    expect_identical(doc_get_details(doc), det)
})
