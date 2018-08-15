#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Fun-toRd.R`')
#line 127 "/rdtf/documentation/R/Fun-toRd.R"
test_that('.Rd_indent', {#@testing
    x <- c("test strings", "second line")

    expect_identical(.Rd_indent(c("test strings", "second line"))
                    , c("test strings", "second line")
                    )

    expect_identical(.Rd_indent(c("test strings", "second line"), indent=TRUE)
                    , c("  test strings", "  second line"))

    withr::with_options(list( "documentation::Rd::indent" = TRUE
                            , "documentation::Rd::indent.with" = NULL), {
        expect_identical(.Rd_indent(c("test strings", "second line"))
                        , c("  test strings", "  second line"))
    })
    withr::with_options(list( "documentation::indent" = TRUE), {
        expect_identical(.Rd_indent(c("test strings", "second line"))
                        , c("  test strings", "  second line"))
    })
    withr::with_options(list( "indent" = TRUE), {
        expect_identical(.Rd_indent(c("test strings", "second line"))
                        , c("  test strings", "  second line"))
    })
    withr::with_options(list( "documentation::Rd::indent" = TRUE
                            , "documentation::Rd::indent.with" = "    "), {
        expect_identical(.Rd_indent(c("test strings", "second line"))
                        , c("    test strings", "    second line"))
    })
    withr::with_options(list( "documentation::Rd::indent" = TRUE
                            , "documentation::Rd::indent.with" = "\t"), {
        expect_warning(.Rd_indent(c("test strings", "second line"))
                      , class= "documentation-warning-guidelines_violation"
                      )
    })
    expect_warning(.Rd_indent(collapse_nl(x), indent=TRUE)
                  , class = "documentation-warning" )
})
#line 211 "/rdtf/documentation/R/Fun-toRd.R"
test_that('.Rd_strwrap', {#@testing
    x <- stringi::stri_rand_lipsum(1)

    expect_identical(.Rd_strwrap(x, wrap.lines=FALSE, wrap.at=72L), x)
    expect_identical(.Rd_strwrap(x, wrap.lines=TRUE , wrap.at=72L)
                    , base::strwrap(x, 72L)
                    )
    expect_identical(.Rd_strwrap(x, wrap.lines=TRUE, wrap.at=50)
                    , base::strwrap(x, 50)
                    )
    withr::with_options(list( "documentation::Rd::wrap.lines" = TRUE
                            , "documentation::Rd::wrap.at"    = 50), {
        expect_identical( .Rd_strwrap(x)
                        , base::strwrap(x, 50)
                        )
    })

    x <- c("Lorem ipsum"
          , interleave( rep('', 3)
                      , stringi::stri_rand_lipsum(3, start_lipsum = FALSE)
                      )
          )
    expect_identical( .Rd_strwrap(x, wrap.lines=TRUE, wrap.at=72L)
                    , unname(unlist(sapply(x, base::strwrap, 72)))
                    )
    expect_equal(sum(.Rd_strwrap(x, wrap.lines=TRUE, wrap.at=72L) == ''), 3L)


    expect_identical( .Rd_strwrap("   hello\n\nworld", wrap.lines=TRUE, wrap.at=72L)
                    , c("   hello", "", "   world")
                    )
})
#line 263 "/rdtf/documentation/R/Fun-toRd.R"
test_that('Rd_tag', {#! @testing
    expect_error(Rd_tag('test', NULL), "name is not a string")
    expect_error(Rd_tag('test', c('a', 'b')), "name is not a string")
    expect_error(Rd_tag('test', 1), "name is not a string")
    expect_is(Rd_tag('my name', 'name'), "Rd_tag")
    expect_is(Rd_tag('my name', 'name'), "Rd")
    expect_equal( unclass(Rd_tag('my name', 'name')), "\\name{my name}")
    expect_equal( unclass(Rd_tag(c('line1', 'line2'), 'name'))
                , c('\\name{', 'line1', 'line2', '}')
                )
    name <- 'testing'
    expect_equal(unclass(Rd_tag(name)), '\\name{testing}')

    obj <- new('FormattedText', stringi::stri_rand_lipsum(3))
    as.tag <- Rd_tag(obj)
    expect_is(as.tag, 'Rd_tag')
    expect_length(as.tag, 7)
    expect_identical(as.tag[c(1,7)], c('\\obj{', '}'))

    val <- Rd_tag('dest', 'link', opt='pkg')
    expect_is(val, 'Rd')
    expect_identical(unclass(val), "\\link[pkg]{dest}")
})
#line 297 "/rdtf/documentation/R/Fun-toRd.R"
test_that('toRd,person-method', {#! @testing
    object <- person('Andrew', 'Redd', email='andrew.redd@hsc.utah.edu')
    val <- toRd(object)
    expect_is(val, 'Rd')
    expect_equal(unclass(val), 'Andrew Redd \\email{andrew.redd@hsc.utah.edu}')

    object <-c( person('First' , 'Author', email='me1@email.com')
              , person('Second', 'Author', email='me2@email.com')
              )
    val <- toRd(object)
    expect_is(val, 'Rd')
    expect_equal( unclass(val)
                ,
                  'First Author \\email{me1@email.com}' %<<% 'and' %<<%
                  'Second Author \\email{me2@email.com}'
                  )
    expect_equal( toRd( c( person('Andrew', 'Redd', email='andrew.redd@hsc.utah.edu')
                         , person('Drew'  , 'Blue')
                         ) )
                , structure( 'Andrew Redd \\email{andrew.redd@hsc.utah.edu}' %<<%
                             'and Drew Blue'
                           , class='Rd')
                )
})
#line 323 "/rdtf/documentation/R/Fun-toRd.R"
test_that('documentation bibstyle', {#!@testing documentation bibstyle
    object <- citation() %>% structure(class='bibentry')
    default.style <- toRd(object, style='JSS')
    doc.style     <- toRd(object, style='documentation')

    expect_true(default.style != doc.style)
})
#line 333 "/rdtf/documentation/R/Fun-toRd.R"
test_that('toRd,Documentation-Keyword-method', {#! @testing
    obj <- new('Documentation-Keyword', c('utilities', 'character'))
    val <- toRd(obj)
    expect_is(val, 'Rd')
    expect_equal( unclass(val)
                , c('\\keyword{utilities}', '\\keyword{character}'))
})
#line 354 "/rdtf/documentation/R/Fun-toRd.R"
test_that('toRd,FormattedText-method', {#! @testing
    obj <- FormattedText()
    expect_identical(toRd(obj), Rd(character(0)))

    obj <- FormattedText('Hello world!')
    expect_identical(toRd(obj), Rd('Hello world!'))
    expect_false(identical(toRd(obj), obj))

    obj <- FormattedText(stringi::stri_rand_lipsum(3))
    as.rd <- toRd(obj)
    expect_equal(length(as.rd), 5 )
    expect_is(as.rd, 'Rd')
    expect_identical(mode(as.rd), 'character')
})
