#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Fun-toRd-usage.R`')
#line 8 "R/Fun-toRd-usage.R"
test_that('toRd,usage-method', {#@testing
    obj <- as(expression(function_documentation(name, arguments, usage, ...)), 'usage')
    expect_identical(toRd(obj), cl(Rd("\\usage{function_documentation(name, arguments, usage, ...)}"), "Rd_tag"))
    
    obj <- as(expression( value %if% proposition
                        , value %if% proposition %otherwise% alternate
                        ), 'usage')
    expect_identical(toRd(obj)
                    , cl(Rd(c('\\usage{'
                             , 'value \\%if\\% proposition'
                             , 'value \\%if\\% proposition \\%otherwise\\% alternate'
                             , '}')), 'Rd_tag') )
})
