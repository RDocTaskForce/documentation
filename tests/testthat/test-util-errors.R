#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `util-errors.R`')
#line 12 "/rdtf/documentation/R/util-errors.R"
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
#line 37 "/rdtf/documentation/R/util-errors.R"
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
#line 100 "/rdtf/documentation/R/util-errors.R"
test_that('condition', {#@testing
    expect_message( condition('testing', 'message', scope='base'), 'testing')
    expect_message( condition('testing', 'message', scope='base', type='testing')
                  , class = "message-testing"
                  )
    expect_message( condition('testing', 'message', scope='test', type='testing')
                  , class = "test-message-testing"
                  )

    expect_warning( condition('testing', 'warning', scope='base'), 'testing')
    expect_warning( condition('testing', 'warning', scope='base', type='testing')
                  , class = "warning-testing"
                  )
    expect_warning( condition('testing', 'warning', scope='test', type='testing')
                  , class = "test-warning-testing"
                  )

    expect_error( condition('testing', 'error', scope='base'), 'testing')
    expect_error( condition('testing', 'error', scope='base', type='testing')
                , class = "error-testing"
                )
    expect_error( condition('testing', 'error', scope='test', type='testing')
                , class = "test-error-testing"
                )

    tryCatch( condition('testing', 'error', type='testing'
                       , scope = .T(test, my_class, my_method)
                       )
            , condition = function(obj){
                expect_is(obj, 'test-error-testing')
                expect_is(obj, 'test::my_class-error-testing')
                expect_is(obj, 'test::my_class::my_method-error-testing')
                expect_is(obj, 'test-error')
                expect_is(obj, 'test::my_class-error')
                expect_is(obj, 'test::my_class::my_method-error')
                expect_is(obj, 'error-testing')
                expect_is(obj, 'error')
                expect_is(obj, 'condition')
            })
})
#line 155 "/rdtf/documentation/R/util-errors.R"
test_that('doc_condition', {#@testing
    expect_null(doc_condition("test message", NULL))

    expect_message( doc_condition("test message", FALSE, type='testing')
                  , class = "documentation-message-testing" )
    expect_warning( doc_condition("test message", NA   , type='testing')
                  , class = "documentation-warning-testing" )
    expect_error  ( doc_condition("test message", TRUE , type='testing')
                  , class = "documentation-error-testing" )

    expect_null(doc_condition("test message", 'none'))
    expect_message( doc_condition("test message", 'message', type='testing')
                  , class = "documentation-message-testing" )
    expect_warning( doc_condition("test message", 'warning', type='testing')
                  , class = "documentation-warning-testing" )
    expect_error  ( doc_condition("test message", 'error'  , type='testing')
                  , class = "documentation-error-testing" )
})
#line 183 "/rdtf/documentation/R/util-errors.R"
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
#line 207 "/rdtf/documentation/R/util-errors.R"
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
#line 230 "/rdtf/documentation/R/util-errors.R"
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
#line 257 "/rdtf/documentation/R/util-errors.R"
test_that('doc_no_src', {#@testing
    expect_error(doc_no_src(), class='documentation-error-no_src')
})
#line 270 "/rdtf/documentation/R/util-errors.R"
test_that('no_doc_comments', {#@testing
    expect_null(no_doc_comments('testing', NULL))
    expect_message( no_doc_comments('testing', FALSE)
                  , class="documentation-message-no_comments")
    expect_warning( no_doc_comments('testing', NA)
                  , class="documentation-warning-no_comments")
    expect_error  ( no_doc_comments('testing', TRUE)
                  , class="documentation-error-no_comments")

    expect_null(no_doc_comments('testing', 'none'))
    expect_message( no_doc_comments('testing', 'message')
                  , class="documentation-message-no_comments")
    expect_warning( no_doc_comments('testing', 'warning')
                  , class="documentation-warning-no_comments")
    expect_error  ( no_doc_comments('testing', 'error')
                  , class="documentation-error-no_comments")
})
#line 318 "/rdtf/documentation/R/util-errors.R"
test_that('doc_error_bad_argument', {#@testing
    f <- function(a){
        doc_error_bad_argument(a, 'logical')
    }
    expect_error( f('hi'), class = "documentation-error-invalid_argument")

})
