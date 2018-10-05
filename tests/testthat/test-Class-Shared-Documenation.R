#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Class-Shared-Documenation.R`')
#line 22 "R/Class-Shared-Documenation.R"
test_that('Class Shared-Documenation', {#@testing Class Shared-Documenation
    hw <- function(){print("hello world")}

    docs <- function_documentation("hw", title = "Hello World")
    shared.docs <- shared(docs=docs)
    expect_is(shared.docs, 'Documentation')
    expect_is(shared.docs, 'Shared-Documentation')
})
