#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Fun-toRd-shiny.R`')
#line 118 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('make_simple_html_converter', {#@testing
    test_fun <- make_simple_html_converter('htmltag', 'rdtag')
    expect_identical( formals(test_fun)
                    , as.pairlist(alist(html=, ...=))
                    )
    expect_identical( deparse(body(test_fun))
                    , "Rd_tag(html_to_Rd(html$children), \"rdtag\")"
                    )
    expect_true(is_documented('test_fun', environment(), complete=FALSE))

    html <- htmltools::tag('htmltag', varArgs = list('content'))
    val <- test_fun(html)
    expect_is(val, 'Rd_tag')
    expect_equal(unclass(val), "\\rdtag{content}")
})
#line 143 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('html_simple_extractor', {#@testing
    html <- htmltools::tag('htmltag', varArgs = list('content'))
    expect_warning( val <- html_simple_extractor(html, extraction.condition='warn')
                  , class =  "documentation-warning-html_to_Rd-html_extraction")
    expect_is(val, 'Rd')
    expect_equal(unclass(val), "content")
})
#line 158 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('html_get_type', {#@testing
    e <- htmltools::em('with some emphatic text.')
    a <- htmltools::tags$p( "Some paragraph text", e)
    b <- htmltools::code("plot(rnorm(100))")
    x <- htmltools::tags$div( a, b)

    expect_identical(html_get_type(x), 'div')
    expect_identical(html_get_type(x$children), c('p', 'code'))
})
#line 179 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('is_rd_link', {#@testing
    expect_true(is_rd_link(''))
    expect_true(is_rd_link('=abc-class'))
    expect_true(is_rd_link('terms.object'))
    expect_true(is_rd_link('base:abc'))
    expect_false(is_rd_link("http://r-project.org"))
})
#line 199 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('html_to_Rd.a', {#@testing
    a <- htmltools::a("some text", href="https://r-project.org")

    val <- html_to_Rd(htmltools::a("some text", href="https://r-project.org"))
    expect_is(val, 'Rd_tag')
    expect_identical(unclass(val), "\\href{https://r-project.org}{some text}")

    val <- html_to_Rd(htmltools::a("some text", href="abc"))
    expect_is(val, 'Rd_tag')
    expect_identical(unclass(val), "\\link[abc]{some text}")

    val <- html_to_Rd(htmltools::a("some text", href="=abc-class"))
    expect_is(val, 'Rd_tag')
    expect_identical(unclass(val), "\\link[=abc-class]{some text}")

    val <- html_to_Rd(htmltools::a("some text", href="pkg:dest"))
    expect_is(val, 'Rd_tag')
    expect_identical(unclass(val), "\\link[pkg:dest]{some text}")

    val <- html_to_Rd(htmltools::a("some text", href="=somewhere"))
    expect_is(val, 'Rd_tag')
    expect_identical(unclass(val), "\\link[=somewhere]{some text}")


    expect_error( html_to_Rd(htmltools::a("some text", href="somewhere over the rainbow"))
                , class = "documentation-error-html_to_Rd" )
})
#line 239 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('html_to_Rd.abbr', {#@testing
    html <- htmltools::tags$abbr("GPL")
    val <- html_to_Rd(html)
    expect_is(val, 'Rd_tag')
    expect_identical(unclass(val), "\\acronym{GPL}")

    expect_warning(html_to_Rd(htmltools::tags$abbr("not an acronym"))
                   , class = 'documentation-warning-html_to_Rd')
})
#line 267 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('html_to_Rd.b', {#@testing
    html <- htmltools::tags$b("something to bold")
    expect_warning( val <- html_to_Rd(html)
                  , class = "documentation-warning-html_to_Rd-discouraged")
    expect_is(val, 'Rd_tag')
    expect_identical(unclass(val), "\\b{something to bold}")
})
#line 279 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('html_to_Rd.br', {#@testing
    html <- htmltools::tags$br()
    val <- html_to_Rd(html)
    expect_is(val, 'Rd_tag')
    expect_identical(unclass(val), "\\cr")
})
#line 303 "/rdtf/documentation/R/Fun-toRd-shiny.R"
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
#line 395 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('html_to_Rd.dl', {#@testing
    html <- htmltools::tags$dl( htmltools::tags$dt("term1")
                                , htmltools::tags$dd("definition 1.")
                              , htmltools::tags$dt("term2")
                                , htmltools::tags$dd("definition 2.")
                              )
    val <- html_to_Rd(htmltools::tags$dt("my term"))
    expect_is(val, 'Rd_tag')
    expect_equal(unclass(val), "\\dfn{my term}" )

    val <- html_to_Rd(html)
    expect_is(val, 'Rd')
    expect_length(val, 4)
    expect_equal(unclass(val)
                , c( "\\describe{"
                   , "\\item{\\dfn{term1}}{definition 1.}"
                   , "\\item{\\dfn{term2}}{definition 2.}"
                   , "}"
                   ) )
})
#line 442 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('html_to_Rd.img', {#@testing
    html <- htmltools::tags$img(src='test.png', alt ='alternate text', height=100, width=100)
    val <- html_to_Rd(html)
    expect_is(val, 'Rd')
    expect_equal( unclass(val)
                , "\\figure{test.png}{options: alt=\"alternate text\" height=100 width=100}")
})
#line 451 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('html_to_Rd.kbd', {#@testing
    html <- htmltools::tags$kbd("abc")
    val <- html_to_Rd(html)
    expect_is(val, 'Rd')
    expect_equal( unclass(val), "\\kbd{abc}")
})
#line 462 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('html_to_Rd.li', {#@testing
    html <- htmltools::tags$li("some ", htmltools::tags$em('text'), '.')
    val <- html_to_Rd(html)
    expect_is(val, 'Rd')
    expect_equal( unclass(val)
                , "\\item some \\emph{text}.")
})
#line 483 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('html_to_Rd.p', {#@testing
    expect_is(html_to_Rd(htmltools::p()), 'Rd')
    expect_equal(unclass(html_to_Rd(htmltools::p())), character(0))
    expect_is(html_to_Rd(htmltools::p("text")), 'Rd')
    expect_equal(unclass(html_to_Rd(htmltools::p("text"))), c("text", ""))
})
#line 591 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('html_to_Rd.tbody', {#@testing
    x <- htmlTable::htmlTable(head(iris, 10))

    matrix(sample(c(rep('X', 5), rep('O', 4))), 3,3)

    html <-
        with(htmltools::tags, {
            table( thead( tr( th(''), th('C1'), th('C2'), th('C3') ) )
                 , tbody( tr( td('R1'), td('O'), td('X'), td('O') )
                        , tr( td('R2'), td('X'), td('X'), td('O') )
                        , tr( td('R3'), td('O'), td('X'), td('X') )
                        )
                 , tfoot( tr( td('Count'), td('1'), td('3'), td('1') ) )
                 , align = "right|center|center|center"
                 )
        })
    thead <- html$children[[1]]
    tbody <- html$children[[1]]
    tfoot <- html$children[[1]]

    html <- thead$children[[1]]

    html <- thead


})
