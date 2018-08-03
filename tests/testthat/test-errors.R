#! This file was automatically produced by documentation::extract_tests on  2018-08-03 01:24:28
#! changes will be overwritten.
context('tests extracted from file `errors.R`')
#line 10 "/rdtf/documentation/R/errors.R"
test_that('doc_error', {#@testing
    expect_error(doc_error("A documentation error."), "A documentation error.")
    x <- tryCatch( doc_error("A documentation error.")
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
    expect_identical(x, "General documentation error.")
})
#line 35 "/rdtf/documentation/R/errors.R"
test_that('doc_warning', {#@testing
    expect_warning(doc_warning("A documentation warning"), "A documentation warning")
    x <- tryCatch( doc_warning("A documentation warning.")
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
    expect_identical(x, "General documentation warning.")
})
#line 67 "/rdtf/documentation/R/errors.R"
test_that('doc_dnf_error', {#@testing
    expect_error(doc_dnf_error(), "Documentation not found.")
    expect_error(doc_dnf_error("throw me"), "Documentation not found for 'throw me'.")
    x <- tryCatch( doc_dnf_error("hello")
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
#line 88 "/rdtf/documentation/R/errors.R"
test_that('doc_invalid', {#@testing
    expect_error(doc_invalid(), "Documentation is not valid.")
    expect_error(doc_invalid("throw me"), "Documentation for 'throw me' is not valid.")
    x <- tryCatch( doc_invalid("hello")
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
#line 110 "/rdtf/documentation/R/errors.R"
test_that('doc_incomplete', {#@testing
    expect_warning(doc_incomplete(), "Documentation is incomplete.")
    expect_warning(doc_incomplete("hello"), "Documentation is incomplete for 'hello'.")
    x <- tryCatch( doc_incomplete("hello")
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
#line 149 "/rdtf/documentation/R/errors.R"
test_that('doc_no_src', {#@testing
    expect_error(doc_no_src(), class='documentation-error-no_src')
})
