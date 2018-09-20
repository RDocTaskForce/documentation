#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Fun-toRd-shiny.R`')
#line 79 "R/Fun-toRd-shiny.R"
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
#line 109 "R/Fun-toRd-shiny.R"
test_that('html_is_type', {#@testing html_is_type
    a <- htmltools::a('link')
    expect_true(html_is_type(a, 'a'))

    withr::with_options(list(useFancyQuotes=FALSE),
                        expect_equal( see_if(html_is_type(a, 'li'))
                                      , s(FALSE, msg = 'a is of type "a"; expected a "li"')
                        ))
})
#line 131 "R/Fun-toRd-shiny.R"
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
#line 164 "R/Fun-toRd-shiny.R"
test_that('is_rd_link', {#@testing
    expect_true(is_rd_link(''))
    expect_true(is_rd_link('=abc-class'))
    expect_true(is_rd_link('terms.object'))
    expect_true(is_rd_link('base:abc'))
    expect_false(is_rd_link("http://r-project.org"))
})
#line 175 "R/Fun-toRd-shiny.R"
test_that('is_header', {#@testing
    expect_true(is_header(htmltools::tags$h1("yes")))
    expect_false(is_header(htmltools::tags$b("yes")))
    expect_false(is_header("yes"))
})
#line 234 "R/Fun-toRd-shiny.R"
test_that('make_simple_html_converter', {#@testing
    test_fun <- make_simple_html_converter('htmltag', 'rdtag')
    expect_identical( formals(test_fun)
                    , as.pairlist(alist(html=, ...=))
                    )

    expect_identical( trimws(deparse(body(test_fun), 500))
                    , c( '{'
                       , "assert_that(html_is_type(html, \"htmltag\"))"
                       , "Rd_tag(\"rdtag\", content = html_to_Rd(html$children))"
                       , "}"
                       )
                    )
    expect_equal( isTRUE(is_documented('test_fun', environment(), complete=FALSE))
                , .document.generated)

    html <- htmltools::tag('htmltag', varArgs = list('content'))
    val <- test_fun(html)
    expect_is(val, 'Rd_tag')
    expect_identical(val, Rd_tag("rdtag", Rd_text("content")))


    expect_error(test_fun(htmltools::tag('not the right tag', varArgs = list('content'))))

    test_children <- make_simple_html_converter('htmltag', 'rdtag', allowed.children = c('tag1', 'tag2'))
    expect_is(test_children, 'function')

    expect_identical( trimws(deparse(body(test_children), 500))
                    , c( '{'
                       , 'assert_that(html_is_type(html, "htmltag"),' %<<%
                            'html_has_valid_children(html, allowed = c("tag1", "tag2")))'
                       , 'Rd_tag("rdtag", content = html_to_Rd(html$children))'
                       , '}'
                       )
                    )
})
#line 281 "R/Fun-toRd-shiny.R"
test_that('html_simple_extractor', {#@testing
    html <- htmltools::tag('htmltag', varArgs = list('content'))
    expect_warning( val <- html_simple_extractor(html, warn.info.loss='warn')
                  , class =  "documentation-warning-html_to_Rd-info_loss")
    expect_is(val, 'Rd')
    expect_identical(val, Rd(Rd_text("content")))
})
#line 304 "R/Fun-toRd-shiny.R"
test_that('html_to_Rd.default', {#@testing
    expect_error( html_to_Rd(1L), class='documentation-error')
})
#line 313 "R/Fun-toRd-shiny.R"
test_that('html_to_Rd.character', {#@testing
    val <- html_to_Rd.character('content')
    expect_is_exactly(val, 'Rd_TEXT')
    expect_identical(val, Rd_text("content"))
})
#line 326 "R/Fun-toRd-shiny.R"
test_that('html_to_Rd.list', {#@testing
    html <- htmltools::tag('htmltag', varArgs = list('content'))$children
    lapply(html, html_to_Rd)
    val <- html_to_Rd.list(html)
    expect_is_exactly(val, 'Rd')
    expect_identical(val, Rd(Rd_text('content')))
})
#line 339 "R/Fun-toRd-shiny.R"
test_that('html_to_Rd.Rd', {#@testing
    expect_identical( html_to_Rd(Rd("hi")), Rd("hi"))
    expect_error(html_to_Rd.Rd("text") )
})
#line 410 "R/Fun-toRd-shiny.R"
test_that('html_to_Rd.kbd', {#@testing
    html <- htmltools::tags$kbd("abc")
    val <- html_to_Rd(html)
    expect_is(val, 'Rd')
    expect_identical( val, Rd_tag("kbd", Rd_text("abc")))
})
#line 455 "R/Fun-toRd-shiny.R"
test_that('html_to_Rd.b', {#@testing
    html <- htmltools::tags$b("something to bold")
    expect_warning( val <- html_to_Rd(html)
                  , class = "documentation-warning-html_to_Rd-discouraged")
    expect_is(val, 'Rd_tag')
    expect_identical(collapse0(val), "\\b{something to bold}")
})
#line 480 "R/Fun-toRd-shiny.R"
test_that('html_to_Rd.a', {#@testing
    expect_identical( html_to_Rd(htmltools::a("somewhere"))
                    , Rd_tag("link", Rd_text("somewhere"))
                    )
    html <- a <- htmltools::a("some text", href="https://r-project.org")

    val <- html_to_Rd(htmltools::a("some text", href="https://r-project.org"))
    expect_is(val, 'Rd_tag')
    expect_identical( val
                    , Rd_tag("href"
                            , Rd(Rd_text("https://r-project.org", 'VERB'))
                            , Rd("some text")
                            ) -> expected)

    val <- html_to_Rd(html <- htmltools::a("some text", href="abc"))
    expect_is(val, 'Rd_tag')
    expect_identical(collapse0(val), "\\link[abc]{some text}")

    val <- html_to_Rd(htmltools::a("some text", href="=abc-class"))
    expect_is(val, 'Rd_tag')
    expect_identical(collapse0(val), "\\link[=abc-class]{some text}")

    val <- html_to_Rd(htmltools::a("some text", href="pkg:dest"))
    expect_is(val, 'Rd_tag')
    expect_identical(collapse0(val), "\\link[pkg:dest]{some text}")

    val <- html_to_Rd(htmltools::a("some text", href="=somewhere"))
    expect_is(val, 'Rd_tag')
    expect_identical(collapse0(val), "\\link[=somewhere]{some text}")

    expect_error( html_to_Rd.a(htmltools::a("some text", href="somewhere over the rainbow"))
                , class = "documentation-error-html_to_Rd" )
    expect_error( html_to_Rd(htmltools::a("some text", href="somewhere over the rainbow"))
                , class = "documentation-error-html_to_Rd" )
})
#line 529 "R/Fun-toRd-shiny.R"
test_that('html_to_Rd.abbr', {#@testing
    html <- htmltools::tags$abbr("GPL")
    val <- html_to_Rd(html)
    expect_is(val, 'Rd_tag')
    expect_identical(collapse0(val), "\\acronym{GPL}")
    expect_identical(val, Rd_tag("acronym", Rd_text("GPL")))

    expect_warning( html_to_Rd(htmltools::tags$abbr("not an acronym"))
                  , class = 'documentation-warning-html_to_Rd')
    expect_error( html_to_Rd(with(htmltools::tags, abbr( "not an acronym"
                                                       , b("not valid")))))

})
#line 550 "R/Fun-toRd-shiny.R"
test_that('html_to_Rd.br', {#@testing
    html <- htmltools::tags$br()
    val <- html_to_Rd(html)
    expect_is(val, 'Rd')
    expect_identical(collapse0(val), "\\cr")

    html <- with(htmltools::tags, br('text'))
    expect_error( html_to_Rd.br(html)
                , class = "documentation-error-html_to_Rd-malformed_html")
    expect_error( html_to_Rd(html)
                , class = "documentation-error-html_to_Rd-malformed_html")
})
#line 570 "R/Fun-toRd-shiny.R"
test_that('html_to_Rd.code', {#@testing
    html <- htmltools::code("plot(rnorm(100))")
    rd <- html_to_Rd(html)
    expect_is(rd, 'Rd_tag')
    expect_is(rd, 'Rd')
    expect_true(is_Rd_tag(rd, '\\code'))
    expect_identical(collapse0(rd), "\\code{plot(rnorm(100))}")

    html <- htmltools::code("'a' %in% letters")
    rd <- html_to_Rd(html)
    expect_is(rd, 'Rd_tag')
    expect_is(rd, 'Rd')
    expect_identical(collapse0(rd), "\\code{'a' \\%in\\% letters}")
})
#line 607 "R/Fun-toRd-shiny.R"
test_that('html_to_Rd.div', {#@testing
    txt <- lapply(stringi::stri_rand_lipsum(3), htmltools::tags$p)
    title <- "test title"
    html <- htmltools::tags$div(title=title)
    html$children <- txt
    val <- html_to_Rd(html)
    expect_is(val, 'Rd')
    expect_true(is_Rd_tag(val, "\\section"))

    h <- htmltools::tags$h3("Embedded Title")
    html <- htmltools::tags$div(h, txt)
    val <- html_to_Rd(html)
    expect_is(val, 'Rd')
    expect_true(is_Rd_tag(val, "\\section"))
    expect_length(val, 2L)
    expect_identical(val[[1]], Rd('Embedded Title'))

    ss <-   htmltools::tags$div( htmltools::tags$h4("Sub-section Header")
                               , stringi::stri_rand_lipsum(2))
    sec <- htmltools::tags$div( htmltools::tags$h3("Section Header")
                              , ss)
    val <- html_to_Rd(sec)
    expect_is(val, 'Rd')
    expect_true(is_Rd_tag(val, '\\section'))
    expect_is_exactly(val[[2]], 'Rd')
    expect_true(is_Rd_tag(val[[2]][[1]], '\\subsection'))

    expect_error( html_to_Rd(with(htmltools::tags, div( em("Section Header"), ss)))
                , class="documentation-error-html_to_Rd-malformed_html" )
})
#line 645 "R/Fun-toRd-shiny.R"
test_that('html_to_Rd.dfn', {#@testing
    rd <- html_to_Rd(html <- htmltools::tags$dfn("abc"))
    expect_identical(rd, Rd_tag('dfn', Rd_text("abc")))
})
#line 684 "R/Fun-toRd-shiny.R"
test_that('html_to_Rd.dl', {#@testing
    html <- htmltools::tags$dl( htmltools::tags$dt("term1")
                                , htmltools::tags$dd("definition 1.")
                              , htmltools::tags$dt("term2")
                                , htmltools::tags$dd("definition 2.")
                              )
    val <- html_to_Rd(htmltools::tags$dt("my term"))
    expect_is(val, 'Rd')
    expect_equal(collapse0(val), "my term" )

    val <- html_to_Rd(htmltools::tags$dd("definition 1."))
    expect_identical(val, Rd("definition 1."))

    val <- html_to_Rd(html)
    expect_is(val, 'Rd')
    expect_equal( collapse0(val)
                , "\\describe{" %\%
                  "\\item{term1}{definition 1.}" %\%
                  "\\item{term2}{definition 2.}" %\%
                  "}"
                )

    val <- html_to_Rd(html, indent=TRUE, indent.with='  ')
    expect_is(val, 'Rd')
    expect_equal( collapse0(val)
                , "\\describe{" %\%
                  "  \\item{term1}{definition 1.}" %\%
                  "  \\item{term2}{definition 2.}" %\%
                  "}"
                )


    html <- htmltools::tags$dl( htmltools::tags$dt("term1")
                                , htmltools::tags$dd("definition 1.")
                                , htmltools::tags$dd("definition 2.")
                              , htmltools::tags$dt("term2")
                              )
    expect_error( html_to_Rd(html)
                , class="documentation-error-html_to_Rd-malformed_html" )


})
#line 731 "R/Fun-toRd-shiny.R"
test_that('html_to_Rd.html', {#@testing
    html1 <- htmltools::tags$dl( htmltools::tags$dt("term1")
                                 , htmltools::tags$dd("definition 1.")
                               , htmltools::tags$dt("term2")
                                 , htmltools::tags$dd("definition 2.")
                               )

    html2 <- htmltools::tags$html(html1)

    expect_identical( html_to_Rd(html2)[[1]]
                    , html_to_Rd(html1)
                    )
})
#line 760 "R/Fun-toRd-shiny.R"
test_that('html_to_Rd.img', {#@testing
    html <- htmltools::tags$img(src='test.png', alt ='alternate text', height=100, width=100)
    val <- html_to_Rd(html)
    expect_is(val, 'Rd')
    expect_true(is_Rd_tag(val, '\\figure'))
    expect_equal( collapse0(val)
                , "\\figure{test.png}{options: alt=\"alternate text\" height=100 width=100}")

    html <- htmltools::tags$img(alt ='alternate text', height=100, width=100)
    expect_error(html_to_Rd(html), class="documentation-error")
})
#line 778 "R/Fun-toRd-shiny.R"
test_that('html_to_Rd.li', {#@testing
    html <- htmltools::tags$li("some ", htmltools::tags$em('text'), '.')
    val <- html_to_Rd(html)
    expect_is(val, 'Rd')
    expect_identical( val, Rd( Rd_tag('item'), Rd_text(" some ")
                             , Rd_tag('emph', Rd_text("text"))
                             , Rd_text(".")))
})
#line 794 "R/Fun-toRd-shiny.R"
test_that('html_to_Rd.p', {#@testing
    html <- htmltools::tags$p(stringi::stri_rand_lipsum(1))

    val <- html_to_Rd(html)
    expect_is_exactly(val, 'Rd')
    expect_is(val[[2]], 'Rd_break')

    expect_is(html_to_Rd(htmltools::p()), 'Rd')
    expect_identical(html_to_Rd(htmltools::p()), Rd())
    expect_is(html_to_Rd(htmltools::p("text")), 'Rd')
    expect_equal(collapse0(html_to_Rd(htmltools::p("text"))), c("text\n\n"))
})
#line 817 "R/Fun-toRd-shiny.R"
test_that('html_to_Rd.ol', {#@testing
    html <- htmltools::tags$ol( htmltools::tags$li("First")
                              , htmltools::tags$li("Second")
                              )
    val <- html_to_Rd(html)
    expect_is(val, 'Rd_tag')
    expected <- Rd_tag( "enumerate", .Rd.newline[[1L]]
                      , Rd_tag("item"), Rd_text(" First\n")
                      , Rd_tag("item"), Rd_text(" Second\n")
                      )
    expect_identical( val, expected)

    val <- html_to_Rd(html, indent=TRUE, indent.with='  ')
    expect_identical( as.character(val)
                    , c( "\\enumerate", '{', '\n'
                       , "  ", "\\item", " First\n"
                       , "  ", "\\item", " Second\n"
                       , "}"))
    expect_error( html_to_Rd(htmltools::tags$ol( htmltools::tags$li("First")
                                               , htmltools::tags$dl("Second")
                                               )))
})
#line 950 "R/Fun-toRd-shiny.R"
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
    expect_is_exactly(head, 'Rd')
    expect_length(head, 7L)
    expect_equal(attr(head, 'ncols'), 4L)
    expect_equal(attr(head, 'nrows'), 1L)
    expect_equal(collapse0(head), " \\tab C1 \\tab C2 \\tab C3")

    body <- html_to_Rd(tbody)
    expect_equal( collapse0(body)
                , "R1 \\tab O \\tab X \\tab O\\cr" %\%
                  "R2 \\tab X \\tab X \\tab O\\cr" %\%
                  "R3 \\tab O \\tab X \\tab X"
                )

    expect_message( foot <- html_to_Rd(tfoot)
                  , class =  "documentation-message-html_to_Rd-info_loss")

    expect_equal( collapse0(foot)
                , "Count \\tab 1 \\tab 3 \\tab 1"
                )

    val <- html_to_Rd(html, warn.info.loss='none')
    expect_is(val, 'Rd')
    expect_true(is_Rd_tag(val, '\\tabular'))
    expect_length(val, 2L)
    expect_is_exactly(val[[1]], 'Rd')
    expect_is_exactly(val[[2]], 'Rd')
    expect_equal( collapse0(val)
                , "\\tabular{llll}{" %\%
                  " \\tab C1 \\tab C2 \\tab C3\\cr" %\%
                  "R1 \\tab O \\tab X \\tab O\\cr" %\%
                  "R2 \\tab X \\tab X \\tab O\\cr" %\%
                  "R3 \\tab O \\tab X \\tab X\\cr" %\%
                  "Count \\tab 1 \\tab 3 \\tab 1" %\%
                  "}"  
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


    expect_identical( html_to_Rd(htmltools::tags$thead()), Rd())
    expect_identical( html_to_Rd(htmltools::tags$tbody()), Rd())
    expect_identical( html_to_Rd(htmltools::tags$tfoot()), Rd())

    thead <- with(htmltools::tags,
                  thead( tr( th(''), th('C'), th('D'), th('E') )
                       , tr( th('.'), th('1'), th('2'), th('3') )
                       ))
    val <- html_to_Rd( thead, collapse.lines=TRUE
                     , warn.info.loss = 'none'
                     )
    expect_is(val, 'Rd')
    expect_identical( collapse0(val)
                    , " \\tab C \\tab D \\tab E\\cr" %\%
                      ". \\tab 1 \\tab 2 \\tab 3"
                    )
    val <- html_to_Rd( thead, collapse.lines=FALSE
                     , warn.info.loss = 'none'
                     )
    expect_is(val, 'Rd')
    expect_identical( collapse0(val)
                    , " \\tab C \\tab D \\tab E\\cr" %\%
                      ". \\tab 1 \\tab 2 \\tab 3"
                    )

    tfoot <- with(htmltools::tags,
                tfoot( tr( td('Count'), td('1'), td('2'), td('3') )
                     , tr( td('Total'), td('A'), td('B'), td('C') )
                     ))

    val <- html_to_Rd( tfoot, collapse.lines=TRUE
                     , warn.info.loss = 'none'
                     )
    expect_is(val, 'Rd')
    expect_identical( collapse0(val)
                    , "Count \\tab 1 \\tab 2 \\tab 3\\cr" %\%
                      "Total \\tab A \\tab B \\tab C"
                    )

    val <- html_to_Rd( tfoot, collapse.lines=FALSE
                     , warn.info.loss = 'none'
                     )
    expect_is(val, 'Rd')
    expect_identical( collapse0(val)
                    , "Count \\tab 1 \\tab 2 \\tab 3\\cr" %\%
                      "Total \\tab A \\tab B \\tab C"
                    )
})
#line 1082 "R/Fun-toRd-shiny.R"
test_that('html_to_Rd.ul', {#@testing
    html <- htmltools::tags$ul( htmltools::tags$li("First")
                              , htmltools::tags$li("Second")
                              )
    val <- html_to_Rd(html)
    expect_is(val, 'Rd_tag')
    expect_identical(val, Rd_tag( "itemize", .Rd.newline[[1L]]
                                , Rd_tag("item"), Rd_text(" First\n")
                                , Rd_tag("item"), Rd_text(" Second\n")
                                ))
    expect_equal( collapse0(val)
                , "\\itemize{" %\%
                  "\\item First" %\%
                  "\\item Second" %\%
                  "}"
                )

    indent.with <- Rd_clean_indent('  ')
    val <- html_to_Rd(html, indent=TRUE, indent.with=indent.with)
    expect_is(val, 'Rd_tag')
    expect_identical(val, Rd_tag( "itemize", .Rd.newline[[1L]]
                                , indent.with[[1L]], Rd_tag("item"), Rd_text(" First\n")
                                , indent.with[[1L]], Rd_tag("item"), Rd_text(" Second\n")
                                ))
    expect_equal( collapse0(val)
                , "\\itemize{" %\%
                  "  \\item First" %\%
                  "  \\item Second" %\%
                  "}"
                )

    expect_error( html_to_Rd(htmltools::tags$ol( htmltools::tags$li("First")
                                               , htmltools::tags$dl("Second")
                                               ))
                )
})
#line 1127 "R/Fun-toRd-shiny.R"
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
    rd <- toRd(html, warn.info.loss='none')
    expect_is_exactly(rd, 'Rd')
    expect_is_exactly(rd[[1]], 'Rd_tag')
    
    expect_identical( rd[[1]]
                    , html_to_Rd(html, warn.info.loss='none')
                    )
})
