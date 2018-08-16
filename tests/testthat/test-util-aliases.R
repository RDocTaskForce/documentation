#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `util-aliases.R`')
#line 20 "/rdtf/documentation/R/util-aliases.R"
test_that('s', {#@testing
    msg <- "An failure message"
    val <- s(FALSE, msg, count = 5)
    expect_identical(attributes(val), list(msg=msg, count=5))
    
})
