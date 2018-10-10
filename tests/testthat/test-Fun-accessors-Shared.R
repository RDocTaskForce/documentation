#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Fun-accessors-Shared.R`')
#line 27 "/rdtf/documentation/R/Fun-accessors-Shared.R"
test_that('Shared accessors', {#@testing Shared accessors
    doc <- shared(function_documentation())
    expect_is_exactly(doc, 'Shared-function-Documentation')
    doc_name(doc) <- 'Normal'
    expect_is_exactly(doc, 'Shared-function-Documentation')

    expect_identical(doc_get_name(doc), 'Normal')
    expect_identical(doc$docs@name, substitute(Normal))
})
#line 41 "/rdtf/documentation/R/Fun-accessors-Shared.R"
test_that('doc_has_name,Shared-Documentation-method', {#@testing
    doc <- shared_function_documentation()
    expect_false(doc_has_name(doc))
    doc_name(doc) <- "test"
    expect_true(doc_has_name(doc))
})
