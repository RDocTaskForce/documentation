#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Class-Rd.R`')
#line 66 "R/Class-Rd.R"
test_that('[[.Rd & [.Rd', {#@testing [[.Rd & [.Rd
    test.file <- system.file("examples", "Normal.Rd", package = 'documentation')
    txt <- tools::parse_Rd(test.file)
    txt <- Rd_rm_srcref(txt)
    txt <- Rd_unclass(txt)
    class(txt) <- 'Rd'

    expect_is_exactly(txt, 'Rd')

    expect_is_exactly(txt[['\\arguments']], 'Rd_tag')
    expect_is_exactly(txt[['\\arguments']][[1]], 'Rd_newline')
    expect_is_exactly(txt[['\\arguments']][[2]], 'Rd_indent')
    expect_is_exactly(txt[['\\arguments']][[3]], 'Rd_tag')
    expect_is_exactly(txt[['\\arguments']][[c(3,1)]], 'Rd')
    expect_is_exactly(txt[['\\arguments']][[c(3,1,1)]], 'Rd_TEXT')
    expect_is_exactly(txt[['\\arguments']][[c(3,2,1)]], 'Rd_TEXT')

    expect_is_exactly(txt[['\\arguments']][[3L]], 'Rd_tag')
    expect_is_exactly(txt[['\\arguments']][[3L]][[1L]], 'Rd')

    expect_is_exactly(txt[[2]], "Rd_newline")
    expect_is_exactly(txt[[c(48, 11)]], "Rd_TEXT")
})
#line 102 "R/Class-Rd.R"
test_that('[[.Rd & [.Rd', {#@testing [[.Rd & [.Rd
    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'documentation'))
    expect_valid(txt)
    expect_true(is.list(txt))

    expect_is(txt[[10]], "Rd_tag")
    expect_is(txt[[10]], "Rd")
    expect_valid(txt[[10]])

    val <- txt[1]

    expect_is_exactly(val, 'Rd')
    expect_length(val, 1L)
    expect_true(is.list(val))
    expect_false(is.list(txt[[1]]))
    expect_valid(is.list(txt[1]))
    expect_valid(is.list(txt[[1]]))

    char <- as.character(val)
    expect_identical(char, "% File src/library/stats/man/Normal.Rd")

    expect_identical( as.character(txt[[10]])
                    , c("\\name", "{", "Normal", "}"))
    expect_identical( as.character(txt[[10]])
                    , as.character(txt[ 10 ])
                    )


    expect_is_exactly(txt[[2L]], 'Rd_newline')
    expect_is_exactly(txt[['\\arguments']], 'Rd_tag')
    expect_is_exactly(txt[['\\arguments']][[2L]], 'Rd_indent')

    expect_identical( txt[['\\seealso']]
                    , txt[[46L]]
                    )
    expect_error(txt[['bibbidy']], ._("tag %s not found", sQuote("bibbidy")))
    expect_error( txt[['\\alias']]
                , "multiple elements matching tag")

    expect_identical( txt['\\alias']
                    , txt[c(12, 14, 16, 18, 20)]
                    )
    expect_identical( class(.Rd.newline[[1]])
                    , c('Rd_newline', 'Rd_TEXT', 'Rd_tag', 'Rd')
                    )
})
#line 153 "R/Class-Rd.R"
test_that('`[.Rd_tag`', {#@testing
    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'documentation'))

    x <- txt[['\\arguments']]
    expect_is(x, 'Rd_tag')
    expect_identical(attr(x, 'Rd_tag'), '\\arguments')
    y <- x[1:6]
    expect_is(y, 'Rd_tag')
    expect_identical(attr(y, 'Rd_tag'), '\\arguments')
})
#line 234 "R/Class-Rd.R"
test_that('is_Rd_newline', {#@testing
    expect_true(is_Rd_newline(.Rd.newline))
    expect_true(is_Rd_newline(.Rd.newline[[1]]))
    expect_false(is_Rd_newline(.Rd.newline[[1]][[1]]))
    expect_true(is_Rd_newline(.Rd.code.newline))
    expect_true(is_Rd_newline(.Rd.code.newline[[1]]))
    expect_true(is_Rd_newline(.Rd.code.newline[[1]]))
    expect_false(is_Rd_newline(.Rd(.Rd.newline)))
})
#line 266 "R/Class-Rd.R"
test_that('Rd_is_all_text', {#@testing
    x <- c("Lorem ipsum", stringi::stri_rand_lipsum(3, start_lipsum = FALSE))
    x <- .Rd_strwrap(collapse(x, '\n\n'), wrap.lines = TRUE, wrap.at = 50)
    expect_is(x, "Rd")
    expect_is_not(x, "Rd_TEXT")

    expect_true(Rd_is_all_text(x))
    expect_true(Rd_is_all_text(x[[1]]))

    y <- s(list(x), Rd_tag='test', class=c('Rd_tag', 'Rd'))

    expect_false(Rd_is_all_text(y))
    expect_identical( validate_that(Rd_is_all_text(y))
                    , "`y` has a bad element at position 1 which is not a `TEXT`" %<<%
                      "type for Rd. It is a" %<<%
                      dQuote('Rd')
                    )
    y <- s(list( Rd_rcode('some(code)')
               , s( list(Rd_symb("some"))
                  , Rd_tag="\\keyword"
                  , class=c("Rd_tag", 'Rd'))
               ), class='Rd')
    expect_identical( validate_that(Rd_is_all_text(y))
                    , "`y` has bad elements at positions 1 and 2 which are not a `TEXT`" %<<%
                      "type for Rd"
                    )
})
#line 297 "R/Class-Rd.R"
test_that('Rd_spans_multiple_lines', {#@testing
    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'documentation'))
    expect_true(Rd_spans_multiple_lines(txt))
    expect_true(Rd_spans_multiple_lines(txt[['\\arguments']]))
    expect_false(Rd_spans_multiple_lines(txt[['\\arguments']][[3L]]))

    expect_false(Rd_spans_multiple_lines(Rd_text("hello world\n")))
    expect_true(Rd_spans_multiple_lines(Rd_text("hello\nworld\n")))

    x <- txt[[38]][2]
    expect_true(Rd_spans_multiple_lines(x))
    expect_false(Rd_spans_multiple_lines(unclass(x)))

    x <- c(.Rd.code.newline
          , Rd_rcode('value \\%if\\% proposition')
          , .Rd.code.newline)
    expect_true(Rd_spans_multiple_lines(x))
})
#line 319 "R/Class-Rd.R"
test_that('Rd_ends_with_newline', {#@testing
    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'documentation'))
    expect_true(Rd_ends_with_newline(txt))

    # ends.with.newline <- purrr::map_lgl(txt, Rd_ends_with_newline)
    # spans.multiple.lines <- purrr::map_lgl(txt, Rd_spans_multiple_lines)
    # expect_false(any(ends.with.newline & !spans.multiple.lines))

    x <- txt[[38]]

    expect_true(Rd_ends_with_newline(x))
    expect_false(Rd_ends_with_newline(x, TRUE))
})
#line 336 "R/Class-Rd.R"
test_that('Rd_starts_with_newline', {#@testing
    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'documentation'))
    expect_false(Rd_starts_with_newline(txt))
    expect_true(Rd_starts_with_newline(txt[['\\arguments']]))
    expect_false(Rd_starts_with_newline(txt[['\\arguments']], TRUE))
})
#line 361 "R/Class-Rd.R"
test_that('Rd_split', {#@testing
    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'documentation'))

    val <- Rd_split(txt)
    expect_is(val, 'list')
    expect_is_not(val, 'Rd')

    expect_all_inherit(val, 'Rd')
    expect_false(all_are(val, 'Rd'))


    x <- txt['\\examples'][[1]]
    y <- Rd_split(x)
    expect_identical(y[[1]], x[[1]])
    expect_length(y, 29L)
})
#line 399 "R/Class-Rd.R"
test_that('compact_Rd', {#@testing
    l <- s(list( .Rd.newline
               , Rd_text('text')
               , .Rd.newline
               , s(list(s(list(Rd_symb('symb')), Rd_tag='tag'
                         , class=c('Rd_tag', 'Rd'))
                       , .Rd.newline
                       ), class='Rd')
               ), class='Rd')
    m <-  compact_Rd(l, recurse=TRUE)

    expect_length(l, 4L)
    expect_length(m, 5L)

    expect_is(m, 'Rd')
    expect_is(m[[1L]], 'Rd_newline')
    expect_is(m[[2L]], 'Rd_TEXT')
    expect_is(m[[3L]], 'Rd_newline')
    expect_is(m[[4L]], 'Rd_tag')
    expect_is(m[[5L]], 'Rd_newline')
})
#line 446 "R/Class-Rd.R"
test_that('Rd', {#@testing
    a <- "test"
    b <- Rd(a)
    expect_is_exactly(b, 'Rd')
    expect_is(b[[1]], 'Rd_TEXT')

    a <- stringi::stri_rand_lipsum(3)
    b <- Rd(collapse(a, '\n\n'), wrap.lines=TRUE)
    expect_is_exactly(b, 'Rd')
    expect_identical(mode(b), 'list')
    expect_true(length(b) > 5)

    c <- Rd(a, wrap.lines=FALSE)
    expect_is_exactly(c, 'Rd')
    d <- Rd(c, wrap.lines=TRUE)
    expect_is_exactly(d, 'Rd')
    expect_identical(c, d)

    expect_error(Rd(NULL))

    expect_is(Rd(), 'Rd')
    expect_length(Rd(), 0L)

    x <- Rd(collapse(stringi::stri_rand_lipsum(3), '\n\n'), wrap.lines=TRUE)
    expect_is_exactly(x, 'Rd')
    expect_is_exactly(x[[1L]], 'Rd_TEXT')
    expect_true(all_inherit(x, c('Rd_TEXT', 'Rd_newline')))


    x <- Rd(Rd_text('text'))
    expect_is_exactly(x, 'Rd')
    expect_is_exactly(x[[1]], 'Rd_TEXT')
})
#line 479 "R/Class-Rd.R"
test_that('Class-Rd', {#@testing Class-Rd
    x <- cl('text', 'Rd')
    expect_is(x, 'Rd')

    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'documentation'))
    expect_is(txt, 'Rd')
    expect_true(validObject(txt))
})
#line 519 "R/Class-Rd.R"
test_that('Rd_text', {#@testing
    val <- Rd_text('testing')
    expect_is(val, 'Rd')
    expect_is(val, 'Rd_tag')
    expect_is(val, 'Rd_TEXT')

    expect_true(is.character(val))
    expect_false(is.list(val))

    val <- Rd_text('some(code)', 'RCODE')
    expect_is(val, 'Rd_RCODE')
    val <- Rd_text('some(code)', 'R')
    expect_is(val, 'Rd_RCODE')

    x <- Rd_text(collapse(stringi::stri_rand_lipsum(3), '\n\n'))
    expect_is(x, 'Rd')
    expect_is(x, 'Rd_tag')
    expect_is(x, 'Rd_TEXT')

    x <- Rd_text(c( 'hello', '\n', ' big', '\n', '  wide', '\n', '   world'))
    expect_is(x, 'Rd')
    expect_is(x, 'Rd_TEXT')

    expect_length(x, 7L)
    # expect_all_inherit(x[c(3,6,9,12)], 'Rd_newline')
    # expect_all_inherit(x[c(4,7,10)], 'Rd_indent')
    # expect_all_inherit(x[-1], c('Rd_TEXT', 'Rd_newline'))

    x <- Rd_text("     hello world")
    expect_is(x, 'Rd')
    expect_is(x, 'Rd_TEXT')
    expect_length(x, 1L)
    # expect_is(x[[1]], 'Rd_indent')
    # expect_is(x[[2]], 'Rd_TEXT')
    # expect_is(x[[3]], 'Rd_newline')
})
#line 570 "R/Class-Rd.R"
test_that('Rd_rcode, Rd_symb, and Rd_comment', {#@testing Rd_rcode, Rd_symb, and Rd_comment
    expect_error(Rd_comment("testing"))
    expect_is(Rd_comment("% comment"), "Rd_COMMENT")
    expect_equal(attr(Rd_comment("% comment"), 'Rd_tag'), "COMMENT")
    expect_is(Rd_rcode("some(code)"), "Rd_RCODE")
    expect_equal(attr(Rd_rcode("some(code)"), 'Rd_tag'), "RCODE")
    expect_is(Rd_symb("name"), "Rd_VERB")
    expect_equal(attr(Rd_symb("name"), 'Rd_tag'), "VERB")

    a <- Rd_rcode("require(graphics)\n")
    expect_is_exactly(a, 'Rd_RCODE')
    expect_length(a, 1L)
})
#line 623 "R/Class-Rd.R"
test_that('Rd_tag', {#! @testing
    expect_error(Rd_tag(NULL, 'test'), "tag is not a string")
    expect_error(Rd_tag(c('a', 'b'), 'test'), "tag is not a string")
    expect_error(Rd_tag(1, 'test'), "tag is not a string")
    expect_is(Rd_tag('name', Rd_text('my name')), "Rd_tag")
    expect_is(Rd_tag('name', Rd_text('my name')), "Rd")
    expect_identical( Rd_tag('name', Rd_text('my name'))
                    , s( list(Rd_text("my name"))
                       , Rd_tag = "\\name"
                       , class  = c('Rd_tag', 'Rd')
                       ))

    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'documentation'))
    e <- txt[['\\arguments']][[3L]]
    attr(e, 'srcref') <- NULL
    for (i in seq_along(e)) attr(e[[i]][[1L]], 'srcref') <- NULL

    expect_identical(mode(e), 'list')
    expect_length(e, 2)

    x <- Rd_tag('item', Rd(Rd_text('arg')), Rd(Rd_text("an agrument")))
    expect_length(x, 2L)

    expect_equal( as.character(x <- Rd_tag('name', Rd_text(c('line1', 'line2'))))
                , c('\\name', '{', 'line1', 'line2', '}')
                )

    val <- Rd_tag('link', Rd_text('dest'), opt=Rd_text('pkg'))
    expect_is(val, 'Rd')
    expect_identical(collapse0(as.character(val)), "\\link[pkg]{dest}")


    content <- Rd_canonize(Rd(collapse(stringi::stri_rand_lipsum(3), '\n\n')))
    tag <- Rd_tag( 'description', content=content
                 , wrap.lines = TRUE, wrap.at = 72
                 , indent=TRUE, indent.with = ' '
                 )
    expect_true(is_Rd_tag(tag, '\\description'))
    expect_true(length(tag) > 5L)
    expect_equal(substr(tag[[2]], 1, 13)[[1]], '  Lorem ipsum')
})
#line 710 "R/Class-Rd.R"
test_that('Rd_* tags', {#@testing Rd_* tags
    rd <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'documentation'))
    txt <- Rd_rm_srcref(rd)

    expect_identical(Rd_alias('Normal'), txt[[12L]])

    expect_identical(Rd_author(Rd('My Name')), Rd_author(Rd_text('My Name')))

    expect_equal( val <- Rd_arguments( Rd_item("x, q", "vector of quantiles.")
                              , Rd_item('p', "vector of probabilities.")
                              , indent = TRUE
                              , indent.with = space(2)
                              ), x <- txt[['\\arguments']][1:7])

    desc <- Rd_description( .Rd.newline
                          , Rd_text("  Density, distribution function, quantile function and random\n")
                          , Rd_text("  generation for the normal distribution with mean equal to ")
                            ,  Rd_tag('code', Rd_rcode('mean')), .Rd.newline
                          , Rd_text("  and standard deviation equal to ")
                            , Rd_tag('code', Rd_rcode('sd'))
                          , Rd_text(".\n")
                          )
    expect_identical( collapse0(as.character(desc))
                    , collapse0(as.character(txt[['\\description']])))

    expect_identical( Rd_examples( .Rd.code.newline
                                 , Rd_rcode("require(graphics)\n")
                                 , .Rd.code.newline
                                 , Rd_rcode("dnorm(0) == 1/sqrt(2*pi)\n")
                                 , Rd_rcode("dnorm(1) == exp(-1/2)/sqrt(2*pi)\n")
                                 , Rd_rcode("dnorm(1) == 1/sqrt(2*pi*exp(1))\n")
                                 )
                      , txt[[52]][1:6] )
    expect_identical( Rd_examples( content=Rd( .Rd.code.newline
                                             , Rd_rcode("require(graphics)\n")
                                             , .Rd.code.newline
                                             , Rd_rcode("dnorm(0) == 1/sqrt(2*pi)\n")
                                             , Rd_rcode("dnorm(1) == exp(-1/2)/sqrt(2*pi)\n")
                                             , Rd_rcode("dnorm(1) == 1/sqrt(2*pi*exp(1))\n")
                                             ))
                      , txt[[52]][1:6] )

    expect_identical( Rd_item("x, q", "vector of quantiles.")
                    , txt[['\\arguments']][[3L]]
                    )

    expect_identical( Rd_keyword('distribution'), txt[['\\keyword']])
    expect_identical( Rd_name('Normal'), txt[['\\name']])
    expect_identical(Rd_title('The Normal Distribution'), txt[['\\title']])

    expect_identical(Rd_usage( .Rd.code.newline
                               , Rd_rcode("dnorm(x, mean = 0, sd = 1, log = FALSE)\n")
                               , Rd_rcode("pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)\n")
                               , Rd_rcode("qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)\n")
                               , Rd_rcode("rnorm(n, mean = 0, sd = 1)\n")
    ), txt[['\\usage']])
})
#line 847 "R/Class-Rd.R"
test_that('Rd_lines', {#@testing
    l <- list( Rd_rcode("value \\%if\\% proposition")
             , Rd_rcode("proposition \\%otherwise\\% alternate"))
    exp <- Rd( Rd_rcode("value \\%if\\% proposition\n")
             , Rd_rcode("proposition \\%otherwise\\% alternate\n"))
    val <- Rd_lines(l)
    expect_identical(val, exp)
})
