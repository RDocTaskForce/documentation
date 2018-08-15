#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `util-generics.R`')
#line 71 "/rdtf/documentation/R/util-generics.R"
test_that('is_S3_method_call', {#@testing
    print.my_class <- function(x, ...){return(invisible(is_S3_method_call()))}
    
    val <- print(s(list(), class="my_class"))
    expect_true(val)
    
    val <- print.my_class(s(list(), class="my_class"))
    expect_false(val)
    expect_false(is_S3_method_call())
})
#line 93 "/rdtf/documentation/R/util-generics.R"
test_that('get_S3_method_specialization', {#@testing
    print.my_class <- function(x, ...)return(invisible(get_S3_method_specialization()))
    
    val <- print(s(list(), class="my_class"), which-1)
    expect_equal(val, 'my_class')
    
    val <- print.my_class(s(list(), class="my_class"))
    expect_null(val)
})
