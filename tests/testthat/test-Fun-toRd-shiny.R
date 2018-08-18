#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Fun-toRd-shiny.R`')
#line 79 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('html_get_type', {#@testing
    e <- htmltools::em('with some emphatic text.')
    a <- htmltools::tags$p( "Some paragraph text", e)
    b <- htmltools::code("plot(rnorm(100))")
    x <- htmltools::tags$div( a, b)

    expect_identical(html_get_type(x), 'div')
    expect_identical(html_get_type(x$children), c('p', 'code'))

    html <- htmltools::tags$li('with some emphatic text.')
    class(html) <- c('li', 'shiny.tag')
    expect_identical(html_get_type(html), 'li')

    expect_identical(html_get_type('character'), '')

    expect_error( html_get_type(NULL)
                , class = "documentation-error" )
})
#line 109 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('html_is_type', {#@testing html_is_type
    a <- htmltools::a('link')
    expect_true(html_is_type(a, 'a'))

    withr::with_options(list(useFancyQuotes=FALSE),
                        expect_equal( see_if(html_is_type(a, 'li'))
                                      , s(FALSE, msg = 'a is of type "a"; expected a "li"')
                        ))
})
#line 131 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('html_has_valid_children', {#@testing
    good.html <- with(htmltools::tags, ol(li('hello'), li("world") ))
    expect_true(see_if(html_has_valid_children(good.html, 'li')))

    bad.html <- with(htmltools::tags, ol(li('hello'), dt("term"), dd("definition") ))
    expect_equal( see_if(html_has_valid_children(bad.html, 'li'))
                , s(FALSE, msg="HTML tag ol contains invalid child tags of types dd and dt."))

    bad.html <- with(htmltools::tags, ol(li('hello'), dt("term"), dd("definition"), a("link") ))
    expect_equal( see_if(html_has_valid_children(bad.html, 'li'))
                , s(FALSE, msg="HTML tag ol contains invalid child tags of types a, dd, and dt."))

    bad.html <- with(htmltools::tags, ol(li('hello'), dt("term")))
    expect_equal( see_if(html_has_valid_children(bad.html, 'li'))
                , s(FALSE, msg="HTML tag ol contains invalid child tag of type dt."))

    bad.html <- with(htmltools::tags, ol(li('hello'), dt("term"), dt("term")))
    expect_equal( see_if(html_has_valid_children(bad.html, 'li'))
                , s(FALSE, msg="HTML tag ol contains invalid child tags of type dt."))
})
#line 164 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('is_rd_link', {#@testing
    expect_true(is_rd_link(''))
    expect_true(is_rd_link('=abc-class'))
    expect_true(is_rd_link('terms.object'))
    expect_true(is_rd_link('base:abc'))
    expect_false(is_rd_link("http://r-project.org"))
})
#line 175 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('is_header', {#@testing
    expect_true(is_header(htmltools::tags$h1("yes")))
    expect_false(is_header(htmltools::tags$b("yes")))
    expect_false(is_header("yes"))
})
#line 234 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('make_simple_html_converter', {#@testing
    test_fun <- make_simple_html_converter('htmltag', 'rdtag')
    expect_identical( formals(test_fun)
                    , as.pairlist(alist(html=, ...=))
                    )
    if(F){
    expect_identical( trimws(deparse(body(test_fun), 500))
                    , c( '{'
                       , "assert_that(html_is_type(html, \"htmltag\"))"
                       , "Rd_tag(html_to_Rd(html$children), \"rdtag\")"
                       , "}"
                       )
                    )
    }
    expect_true(is_documented('test_fun', environment(), complete=FALSE))

    html <- htmltools::tag('htmltag', varArgs = list('content'))
    val <- test_fun(html)
    expect_is(val, 'Rd_tag')
    expect_equal(unclass(val), "\\rdtag{content}")

    expect_error(test_fun(htmltools::tag('not the right tag', varArgs = list('content'))))

    test_children <- make_simple_html_converter('htmltag', 'rdtag', allowed.children = c('tag1', 'tag2'))
    expect_is(test_children, 'function')

    if(F){
    expect_identical( trimws(deparse(body(test_children), 500))
                    , c( '{'
                       , 'assert_that(html_is_type(html, "htmltag"),' %<<%
                            'html_has_valid_children(html, allowed = c("tag1", "tag2")))'
                       , 'Rd_tag(html_to_Rd(html$children), "rdtag")'
                       , '}'
                       )
                    )
    }
})
#line 282 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('html_simple_extractor', {#@testing
    html <- htmltools::tag('htmltag', varArgs = list('content'))
    expect_warning( val <- html_simple_extractor(html, warn.info.loss='warn')
                  , class =  "documentation-warning-html_to_Rd-info_loss")
    expect_is(val, 'Rd')
    expect_equal(unclass(val), "content")
})
#line 303 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('html_to_Rd.default', {#@testing
    expect_error( html_to_Rd(1L), class='documentation-error')
})
#line 327 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('html_to_Rd.Rd', {#@testing
    expect_identical( html_to_Rd(Rd("hi")), Rd("hi"))
    expect_error(html_to_Rd.Rd("text") )
})
#line 405 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('html_to_Rd.kbd', {#@testing
    html <- htmltools::tags$kbd("abc")
    val <- html_to_Rd(html)
    expect_is(val, 'Rd')
    expect_equal( unclass(val), "\\kbd{abc}")
})
#line 415 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('html_to_Rd.ol', {#@testing
    html <- htmltools::tags$ol( htmltools::tags$li("First")
                              , htmltools::tags$li("Second")
                              )
    val <- html_to_Rd(html)
    expect_is(val, 'Rd_tag')
    expect_equal( unclass(val)
                  , c( "\\itemize{"
                       , "\\item First"
                       , "\\item Second"
                       , "}"
                  ))

    expect_error( html_to_Rd(htmltools::tags$ol( htmltools::tags$li("First")
                                                 , htmltools::tags$dl("Second")
    ))
    )
})
#line 453 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('html_to_Rd.ul', {#@testing
    html <- htmltools::tags$ol( htmltools::tags$li("First")
                              , htmltools::tags$li("Second")
                              )
    val <- html_to_Rd(html)
    expect_is(val, 'Rd_tag')
    expect_equal( unclass(val)
                , c( "\\itemize{"
                   , "\\item First"
                   , "\\item Second"
                   , "}"
                   ))

    expect_error( html_to_Rd(htmltools::tags$ol( htmltools::tags$li("First")
                                               , htmltools::tags$dl("Second")
                                               ))
                )
})
#line 495 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('html_to_Rd.b', {#@testing
    html <- htmltools::tags$b("something to bold")
    expect_warning( val <- html_to_Rd(html)
                  , class = "documentation-warning-html_to_Rd-discouraged")
    expect_is(val, 'Rd_tag')
    expect_identical(unclass(val), "\\b{something to bold}")
})
#line 521 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('html_to_Rd.small', {#@testing
    html <- htmltools::tags$small("something small")
    expect_warning( val <- html_to_Rd(html)
                  , class = "documentation-warning-html_to_Rd-discouraged")
    expect_is(val, 'Rd')
    expect_identical(unclass(val), "{\\small something small}")

    expect_warning( val <- html_to_Rd(html, size="\\tiny")
                  , class = "documentation-warning-html_to_Rd-discouraged")
    expect_identical(unclass(val), "{\\tiny something small}")

    expect_error(suppressWarnings(val <- html_to_Rd(html, size="\\miniscule")))

    html <- htmltools::tags$small(c("something small", "and another thing"))
    withr::with_options( list("Rd.small.size" = '\\scriptsize'),{
        expect_warning( val <- html_to_Rd(html)
                      , class = "documentation-warning-html_to_Rd-discouraged")
        expect_equal(unclass(val)
                    , c("{\\scriptsize"
                       , "something small"
                       , "and another thing"
                       , "}"
                       ) )
    })

    expect_identical( suppressWarnings(html_to_Rd(htmltools::tags$small()))
                    , Rd(character(0))
                    )

})
#line 568 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('html_to_Rd.a', {#@testing
    expect_identical( html_to_Rd(htmltools::a("somewhere"))
                    , cl(Rd("\\link{somewhere}"), 'Rd_tag')
                    )



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

    expect_error( html_to_Rd.a(htmltools::a("some text", href="somewhere over the rainbow"))
                , class = "documentation-error-html_to_Rd" )
    expect_error( html_to_Rd(htmltools::a("some text", href="somewhere over the rainbow"))
                , class = "documentation-error-html_to_Rd" )
})
#line 617 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('html_to_Rd.abbr', {#@testing
    html <- htmltools::tags$abbr("GPL")
    val <- html_to_Rd(html)
    expect_is(val, 'Rd_tag')
    expect_identical(unclass(val), "\\acronym{GPL}")

    expect_warning( html_to_Rd(htmltools::tags$abbr("not an acronym"))
                  , class = 'documentation-warning-html_to_Rd')
})
#line 634 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('html_to_Rd.br', {#@testing
    html <- htmltools::tags$br()
    val <- html_to_Rd(html)
    expect_is(val, 'Rd')
    expect_identical(unclass(val), "\\cr")

    html <- with(htmltools::tags, br('text'))
    expect_error( html_to_Rd.br(html)
                , class = "documentation-error-html_to_Rd-malformed_html")
    expect_error( html_to_Rd(html)
                , class = "documentation-error-html_to_Rd-malformed_html")
})
#line 654 "/rdtf/documentation/R/Fun-toRd-shiny.R"
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
#line 685 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('html_to_Rd.div', {#@testing
    txt <- stringi::stri_rand_lipsum(3)
    title <- "test title"
    html <- htmltools::tags$div(txt, title=title)
    val <- html_to_Rd(html)
    expect_is(val, 'Rd')
    expect_equal(val[[1]], '\\section{test title}{')
    expect_equal(tail(val, 1), "}")

    h <- htmltools::tags$h3("Embedded Title")
    html <- htmltools::tags$div(h, txt)
    val <- html_to_Rd(html)
    expect_is(val, 'Rd')
    expect_equal(val[[1]], '\\section{Embedded Title}{')

    ss <-   htmltools::tags$div( htmltools::tags$h4("Sub-section Header")
                               , stringi::stri_rand_lipsum(2))
    sec <- htmltools::tags$div( htmltools::tags$h3("Section Header")
                              , ss)
    val <- html_to_Rd(sec)
    expect_is(val, 'Rd')
    expect_equal( val[1:2]
                , c( "\\section{Section Header}{"
                   , "\\subsection{Sub-section Header}{"
                   ) )

    expect_equal( tail(val, 2), c( "}", "}") )

})
#line 746 "/rdtf/documentation/R/Fun-toRd-shiny.R"
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

    html <- htmltools::tags$dl( htmltools::tags$dt("term1")
                                , htmltools::tags$dd("definition 1.")
                                , htmltools::tags$dd("definition 2.")
                              , htmltools::tags$dt("term2")
                              )
    expect_error( html_to_Rd(html)
                , class="documentation-error-html_to_Rd-malformed_html" )


})
#line 781 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('html_to_Rd.html', {#@testing
    html1 <- htmltools::tags$dl( htmltools::tags$dt("term1")
                                 , htmltools::tags$dd("definition 1.")
                               , htmltools::tags$dt("term2")
                                 , htmltools::tags$dd("definition 2.")
                               )

    html2 <- htmltools::tags$html(html1)

    expect_identical( html_to_Rd(html1)
                    , cl(html_to_Rd(html2), 'Rd_tag')
                    )
})
#line 807 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('html_to_Rd.img', {#@testing
    html <- htmltools::tags$img(src='test.png', alt ='alternate text', height=100, width=100)
    val <- html_to_Rd(html)
    expect_is(val, 'Rd')
    expect_equal( unclass(val)
                , "\\figure{test.png}{options: alt=\"alternate text\" height=100 width=100}")

    html <- htmltools::tags$img(alt ='alternate text', height=100, width=100)
    expect_error(html_to_Rd(html), class="documentation-error")
})
#line 823 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('html_to_Rd.li', {#@testing
    html <- htmltools::tags$li("some ", htmltools::tags$em('text'), '.')
    val <- html_to_Rd(html)
    expect_is(val, 'Rd')
    expect_equal( unclass(val)
                , "\\item some \\emph{text}.")
})
#line 839 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('html_to_Rd.p', {#@testing
    expect_is(html_to_Rd(htmltools::p()), 'Rd')
    expect_equal(unclass(html_to_Rd(htmltools::p())), character(0))
    expect_is(html_to_Rd(htmltools::p("text")), 'Rd')
    expect_equal(unclass(html_to_Rd(htmltools::p("text"))), c("text", ""))
})
#line 983 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('html_to_Rd.* table functions', {#@testing html_to_Rd.* table functions
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
    tbody <- html$children[[2]]
    tfoot <- html$children[[3]]

    expect_warning( head <- html_to_Rd(thead)
                  , class =  "documentation-warning-html_to_Rd-info_loss")
    expect_equal(unclass(head), s(" \\tab C1 \\tab C2 \\tab C3", ncols=4L, nrows=1L))

    body <- html_to_Rd(tbody)
    expect_equal( unclass(body)
                , s(c( "R1 \\tab O \\tab X \\tab O\\cr"
                     , "R2 \\tab X \\tab X \\tab O\\cr"
                     , "R3 \\tab O \\tab X \\tab X"
                     )
                   , ncols=4L, nrows=3L))

    expect_message( foot <- html_to_Rd(tfoot)
                  , class =  "documentation-message-html_to_Rd-info_loss")

    expect_equal( unclass(foot)
                , s("Count \\tab 1 \\tab 3 \\tab 1", ncols=4L, nrows=1L)
                )

    val <- html_to_Rd(html, warn.info.loss='none')
    expect_is(val, 'Rd')
    expect_equal(unclass(val)
                , s(c( "\\tabular{llll}{"
                     , " \\tab C1 \\tab C2 \\tab C3\\cr"
                     , "R1 \\tab O \\tab X \\tab O\\cr"
                     , "R2 \\tab X \\tab X \\tab O\\cr"
                     , "R3 \\tab O \\tab X \\tab X\\cr"
                     , "Count \\tab 1 \\tab 3 \\tab 1"
                     )
                   , nrows = 5, ncols = 4)
                )

    html <-
        with(htmltools::tags, {
            table( thead( tr( th(''), th('C1'), th('C2'), th('C3') ) )
                 , tfoot( tr( td('Count'), td('1'), td('3'), td('1') ) )
                 , align = "right|center|center|center"
                 )
        })
    expect_error(html_to_Rd(html), class="documentation-error-html_to_Rd-malformed_html")

    html <-
        with(htmltools::tags, {
            table( thead( tr( th(''), th('C1'), th('C2'), th('C3') ) )
                 , tfoot( tr( td('Count'), td('1'), td('3'), td('1') ) )
                 , align = "right|center|center|center"
                 )
        })
    expect_error(html_to_Rd(html), class="documentation-error-html_to_Rd-malformed_html")


    expect_identical( html_to_Rd(htmltools::tags$tbody())
                    , Rd(character(0))
                    )

    thead <- with(htmltools::tags,
                  thead( tr( th(''), th('C'), th('D'), th('E') )
                       , tr( th('.'), th('1'), th('2'), th('3') )
                       ))
    val <- html_to_Rd( thead, collapse.lines=TRUE
                     , warn.info.loss = 'none'
                     )
    expect_is(val, 'Rd')
    expect_identical( val
                    , s(Rd(" \\tab C \\tab D \\tab E\\cr" %\%
                           ". \\tab 1 \\tab 2 \\tab 3"
                          ), nrows=2L, ncols=4L)
                    )
    val <- html_to_Rd( thead, collapse.lines=FALSE
                     , warn.info.loss = 'none'
                     )
    expect_is(val, 'Rd')
    expect_identical( val
                    , s(Rd(c( " \\tab C \\tab D \\tab E\\cr"
                            , ". \\tab 1 \\tab 2 \\tab 3"
                            )
                          ), nrows=2L, ncols=4L)
                    )

    tfoot <- with(htmltools::tags,
                tfoot( tr( td('Count'), td('1'), td('2'), td('3') )
                     , tr( td('Total'), td('A'), td('B'), td('C') )
                     ))

    val <- html_to_Rd( tfoot, collapse.lines=TRUE
                     , warn.info.loss = 'none'
                     )
    expect_is(val, 'Rd')
    expect_identical( val
                    , s(Rd("Count \\tab 1 \\tab 2 \\tab 3\\cr" %\%
                           "Total \\tab A \\tab B \\tab C"
                          ), nrows=2L, ncols=4L)
                    )

    val <- html_to_Rd( tfoot, collapse.lines=FALSE
                     , warn.info.loss = 'none'
                     )
    expect_is(val, 'Rd')
    expect_identical( val
                    , s(Rd(c( "Count \\tab 1 \\tab 2 \\tab 3\\cr"
                            , "Total \\tab A \\tab B \\tab C"
                            )
                          ), nrows=2L, ncols=4L)
                    )
})
#line 1114 "/rdtf/documentation/R/Fun-toRd-shiny.R"
test_that('toRd,shiny.tag-method', {#@testing
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
    expect_identical( toRd(html, warn.info.loss='none')
                    , html_to_Rd(html, warn.info.loss='none')
                    )
})
