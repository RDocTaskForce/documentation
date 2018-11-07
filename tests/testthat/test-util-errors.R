#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `util-errors.R`')
#line 12 "C:/Users/u0092104/Dropbox/rdtf/documentation/R/util-errors.R"
test_that('doc_error', {#@testing
    expect_error(doc_error("A documentation error."), "A documentation error.")
    x <- catch_condition(test_doc_condition(doc_error, "A documentation error."))
    expect_is(x, "documentation::test_doc_condition-error")
})
#line 21 "C:/Users/u0092104/Dropbox/rdtf/documentation/R/util-errors.R"
test_that('doc_warning', {#@testing
    expect_warning(doc_warning("A documentation warning"), "A documentation warning")
    x <- catch_condition(test_doc_condition(doc_warning, "A documentation warning."))
    expect_is(x, "documentation::test_doc_condition-warning")
})
#line 30 "C:/Users/u0092104/Dropbox/rdtf/documentation/R/util-errors.R"
test_that('doc_message', {#@testing
    expect_message(doc_message("A documentation message"), "A documentation message")
    x <- catch_condition(test_doc_condition(doc_message, "A documentation message"))
    expect_is(x, "documentation::test_doc_condition-message")
})
#line 43 "C:/Users/u0092104/Dropbox/rdtf/documentation/R/util-errors.R"
test_that('doc_dnf_error', {#@testing
    expect_error(doc_dnf_error(), "Documentation not found.")
    expect_error(doc_dnf_error("throw me"), "Documentation not found for 'throw me'.")
    x <- tryCatch( test_doc_condition(doc_dnf_error, "hello")
                 , 'documentation-error-dnf'= function(e)
                        "Documentation not found!"
                 , 'documentation-error-invalid'= function(e)
                        "Documentation is invalid!"
                 , 'documentation-warning-incomplete'= function(w)
                        "Documentation is incomplete"
                 , 'documentation-error' = function(e)
                        "General documentation error."
                 , 'documentation-warning' = function(e)
                        "General documentation warning."
                 )
    expect_identical(x, "Documentation not found!")
})
#line 67 "C:/Users/u0092104/Dropbox/rdtf/documentation/R/util-errors.R"
test_that('doc_invalid', {#@testing
    expect_error(doc_invalid(), "Documentation is not valid.")
    expect_error(doc_invalid("throw me"), "Documentation for 'throw me' is not valid.")
    x <- tryCatch( test_doc_condition(doc_invalid, "hello")
                 , 'documentation-error-dnf'= function(e)
                        "Documentation not found!"
                 , 'documentation-error-invalid'= function(e)
                        "Documentation is invalid!"
                 , 'documentation-warning-incomplete'= function(w)
                        "Documentation is incomplete"
                 , 'documentation-error' = function(e)
                        "General documentation error."
                 , 'documentation-warning' = function(e)
                        "General documentation warning."
                 )
    expect_identical(x, "Documentation is invalid!")
})
#line 90 "C:/Users/u0092104/Dropbox/rdtf/documentation/R/util-errors.R"
test_that('doc_incomplete', {#@testing
    expect_warning(doc_incomplete(), "Documentation is incomplete.")
    expect_warning(doc_incomplete("hello"), "Documentation is incomplete for 'hello'.")
    x <- tryCatch( test_doc_condition(doc_incomplete, "hello")
                 , 'documentation-error-dnf'= function(e)
                        "Documentation not found!"
                 , 'documentation-error-invalid'= function(e)
                        "Documentation is invalid!"
                 , 'documentation-warning-incomplete'= function(w)
                        "Documentation is incomplete"
                 , 'documentation-error' = function(e)
                        "General documentation error."
                 , 'documentation-warning' = function(e)
                        "General documentation warning."
                 )
    expect_identical(x, "Documentation is incomplete")
})
#line 117 "C:/Users/u0092104/Dropbox/rdtf/documentation/R/util-errors.R"
test_that('doc_no_src', {#@testing
    expect_error(test_doc_condition(doc_no_src), class='documentation-error-no_src')
})
#line 130 "C:/Users/u0092104/Dropbox/rdtf/documentation/R/util-errors.R"
test_that('no_doc_comments', {#@testing
    expect_null(test_doc_condition(no_doc_comments, 'testing', NULL))

    expect_null(test_doc_condition(no_doc_comments, 'testing', 'none'))
    expect_message( test_doc_condition(no_doc_comments, 'testing', 'message')
                  , class="documentation-message-no_comments")
    expect_warning( test_doc_condition(no_doc_comments, 'testing', 'warning')
                  , class="documentation-warning-no_comments")
    expect_error  ( test_doc_condition(no_doc_comments, 'testing', 'error')
                  , class="documentation-error-no_comments")
})
#line 170 "C:/Users/u0092104/Dropbox/rdtf/documentation/R/util-errors.R"
test_that('doc_error_bad_argument', {#@testing
    f <- function(a)
        doc_error_bad_argument(a, 'logical')
    expect_error( f('hi'), class = "documentation-error-invalid_argument")

})
