#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Fun-toRd-usage.R`')
#line 16 "R/Fun-toRd-usage.R"
test_that('toRd,usage-method', {#@testing
    obj <- as(expression(function_documentation(name, arguments, usage, ...)), 'usage')
    expect_identical( toRd(obj)
                    , Rd(Rd_usage(Rd_rcode("function_documentation(name, arguments, usage, ...)")))
                    )

    obj <- as(expression( value %if% proposition
                        , value %if% proposition %otherwise% alternate
                        ), 'usage')
    rd <- toRd(obj)
    expect_is_exactly(rd, 'Rd')
    expect_true(is_Rd_tag(rd[[1]], '\\usage'))
    expect_identical( rd
                    , Rd(Rd_usage( .Rd.code.newline
                                 , Rd_rcode('value \\%if\\% proposition\n')
                                 , Rd_rcode('value \\%if\\% proposition \\%otherwise\\% alternate\n')
                                 )))

    expect_identical( toRd(obj, indent=TRUE, indent.with=.Rd.default.indent)
                    , Rd(Rd_usage( .Rd.code.newline
                                 , Rd_rcode('    value \\%if\\% proposition\n')
                                 , Rd_rcode('    value \\%if\\% proposition \\%otherwise\\% alternate\n')
                                 )))
})
#line 56 "R/Fun-toRd-usage.R"
test_that('toRd,usage/S3method-method', {#@testing
    ex <- expression(html_to_Rd.em(html, ...))
    obj <- new('usage/S3method', ex, generic = 'html_to_Rd', signature = 'em')
    rd <- toRd(obj)
    expect_is_exactly(rd, 'Rd')
    expect_true(is_Rd_tag(rd[[1]], '\\usage'))
    expect_true(is_Rd_tag(rd[[c(1,1)]], '\\S3method'))
    expect_identical(collapse0(rd), "\\usage{\\S3method{html_to_Rd}{em}(html, ...)}")
})
#line 83 "R/Fun-toRd-usage.R"
test_that('toRd,usage/S4method-method', {#@testing
    # Taken from stat4 package file src/library/stats4/man/plot-methods.Rd
    #
    # \S4method{plot}{profile.mle,missing}(x, levels
    #   , conf = c(99, 95, 90, 80, 50)/100, nseg = 50
    #   , absVal = TRUE, \dots)
    ex <- expression(plot(x, levels, conf = c(99, 95, 90, 80, 50)/100, nseg = 50, absVal = TRUE, ...))
    obj <- new('usage/S4method', ex, generic = 'plot'
              , signature = signature(x='profile.mle', y='missing')
              )
    rd <- toRd(obj)
    expect_is_exactly(rd, 'Rd')
    expect_true(is_Rd_tag(rd[[1]], '\\usage'))
    expect_true(is_Rd_tag(rd[[c(1,1)]], '\\S4method'))
    expect_identical( collapse0(rd)
                    , "\\usage{\\S4method{plot}{profile.mle,missing}" %<<<%
                      "(x, levels, conf = c(99, 95, 90, 80, 50)/100" %<<<%
                      ", nseg = 50, absVal = TRUE, ...)}"
                    )
})
