#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Fun-toRd-shiny.R`')
#line 78 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('html_to_Rd.code', {#@testing
    html <- htmltools::code("plot(rnorm(100))")
    rd <- html_to_Rd(html)
    expect_is(rd, 'Rd_tag')
    expect_is(rd, 'Rd')
    expect_identical(unclass(rd), "\\code{plot(rnorm(100))}")

    html <- htmltools::code("'a' %in% letters")
    rd <- html_to_Rd(html)
    expect_is(rd, 'Rd_tag')
    expect_is(rd, 'Rd')
    expect_identical(unclass(rd), "\\code{'a' \\%in\\% letters}")
})
#line 107 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('html_to_Rd.p', {#@testing
    expect_is(html_to_Rd(htmltools::p()), 'Rd')
    expect_equal(unclass(html_to_Rd(htmltools::p())), character(0))
    expect_is(html_to_Rd(htmltools::p("text")), 'Rd')
    expect_equal(unclass(html_to_Rd(htmltools::p("text"))), c("text", ""))
})
