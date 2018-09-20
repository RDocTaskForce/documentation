#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Fun-toRd-usage.R`')
#line 15 "R/Fun-toRd-usage.R"
test_that('toRd,usage-method', {#@testing
    obj <- as(expression(function_documentation(name, arguments, usage, ...)), 'usage')
    expect_identical( toRd(obj)
                    , Rd(Rd_usage(Rd_code("function_documentation(name, arguments, usage, ...)")))
                    )

    obj <- as(expression( value %if% proposition
                        , value %if% proposition %otherwise% alternate
                        ), 'usage')
    rd <- toRd(obj)
    expect_is_exactly(rd, 'Rd')
    expect_true(is_Rd_tag(rd[[1]], '\\usage'))
    expect_identical( rd
                    , Rd(Rd_usage( .Rd.code.newline
                                 , Rd_code('value \\%if\\% proposition'), .Rd.code.newline
                                 , Rd_code('value \\%if\\% proposition \\%otherwise\\% alternate'), .Rd.code.newline
                                 )))

    .Rd_indent(rd, indent=TRUE, indent.with=.Rd.default.indent)

    rd <- toRd(obj, indent=TRUE, indent.with=.Rd.default.indent)
    expect_identical( rd
                    , Rd(Rd_usage( .Rd.code.newline
                                 , .Rd.default.indent, Rd_code('value \\%if\\% proposition'), .Rd.code.newline
                                 , .Rd.default.indent, Rd_code('value \\%if\\% proposition \\%otherwise\\% alternate'), .Rd.code.newline
                                 )))
})
