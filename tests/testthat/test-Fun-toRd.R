#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Fun-toRd.R`')
#line 25 "R/Fun-toRd.R"
test_that('toRd@valueClass', {#@testing
    val <- toRd('character')
    expect_identical(val, Rd("character"))

    val <- toRd(c( "use the \\backslash to escape."
                 , "and '{}' to group."
                 ))
    expect_is(val, 'Rd')
    expect_identical( val
                    , Rd(Rd_text("use the \\\\backslash to escape." %<<<%
                                 "and '\\{\\}' to group."
                                 )))
    obj <-
        c( person('Andrew', 'Redd', email='andrew.redd@hsc.utah.edu')
                                  , person('Drew'  , 'Blue')
                                  )
    expect_identical( toRd(obj)
                    , Rd(Rd_text("Andrew Redd \\email{andrew.redd@hsc.utah.edu} and Drew Blue"))
                    )
})
#line 45 "R/Fun-toRd.R"
test_that('toRd@valueClass', {#@testing
    toRd.test_class <- function(obj, ...)obj
    expect_error( toRd(cl(1L, 'test_class'))
                , class = "documentation-error")
})
#line 152 "R/Fun-toRd.R"
test_that('Rd_canonize_code', {#@testing
    x <- Rd_usage( .Rd.code.newline
                 , Rd_rcode('value \\%if\\% proposition'), .Rd.code.newline
                 , Rd_rcode('value \\%if\\% proposition \\%otherwise\\% alternate'), .Rd.code.newline
                 )
    y <- x[2:3]

    expect_identical( Rd_canonize_code(y)
                    , Rd_usage(Rd_rcode("value \\%if\\% proposition\n"))
                    )
    expect_identical(Rd_canonize_text(y), y)


    expect_identical( val<-Rd_canonize_code(x)
                    , Rd_usage( .Rd.code.newline
                              , Rd_rcode("value \\%if\\% proposition\n")
                              , Rd_rcode("value \\%if\\% proposition \\%otherwise\\% alternate\n")
                              ) )

})
#line 248 "R/Fun-toRd.R"
test_that('.Rd_indent', {#@testing
    x <- .Rd_strwrap( collapse(stringi::stri_rand_lipsum(3), '\n\n'), wrap.lines = TRUE)
    expect_is(x, 'Rd')
    expect_true(length(x)> 5)

    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'documentation'))
    txt <- Rd_rm_srcref(txt)
    x <- txt[['\\examples']]

    expect_identical(.Rd_indent(x, indent=FALSE), x)

    val <- .Rd_indent(x, indent=TRUE)
    expect_is(val, 'Rd')
    expect_is(val, 'Rd_tag')
    expect_identical( stringi::stri_split_lines1(collapse0(as.character(val)))
                    , stringi::stri_split_lines1(collapse0(as.character(x))) %>%
                        ifelse(. %in% c('\\examples{', '}', ''), ., paste0(space(4), .))
                    )
    expect_equal(length(val), length(x))

    x <- txt[['\\arguments']][[9]]
    val <- .Rd_indent(x, indent=TRUE, indent.with='  ')
    exp <- Rd_item('n', Rd( x[[2]][1:3]
                          , Rd_text(paste0('  ', x[[2]][[4]]))
                          ))
    expect_identical(val, exp)

    x <- Rd_untag(txt[['\\arguments']][8:10])
    val <- .Rd_indent(x, indent=TRUE, indent.with='  ', indent.first=FALSE)
    expect_equal(collapse0(val)
                , "  \\item{n}{number of observations. If \\code{length(n) > 1}, the length" %\%
                  "      is taken to be the number required.}\n" )

    x <- txt[['\\arguments']]
    val <- .Rd_indent(x, indent=TRUE, indent.with='  ')
    expect_equal( collapse0(val)
                , "\\arguments{" %\%
                  "    \\item{x, q}{vector of quantiles.}" %\%
                  "    \\item{p}{vector of probabilities.}" %\%
                  "    \\item{n}{number of observations. If \\code{length(n) > 1}, the length" %\%
                  "      is taken to be the number required.}" %\%
                  "    \\item{mean}{vector of means.}" %\%
                  "    \\item{sd}{vector of standard deviations.}" %\%
                  "    \\item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}" %\%
                  "    \\item{lower.tail}{logical; if TRUE (default), probabilities are" %\%
                  "      \\eqn{P[X \\le x]} otherwise, \\eqn{P[X > x]}.}" %\%
                  "}"
                )
})
#line 297 "R/Fun-toRd.R"
test_that('.Rd_indent specification by options', {#@testing .Rd_indent specification by options
    x <- Rd_canonize(Rd(Rd_text(c("test strings\n", "second line"))))
    withr::with_options(list( "documentation::Rd::indent" = TRUE
                            , "documentation::Rd::indent.with" = NULL), {
        expect_identical( .Rd_indent(x)
                        , Rd(Rd_text( .Rd.default.indent %<<<% "test strings\n")
                            ,Rd_text( .Rd.default.indent %<<<% "second line")
                            ))
    })
    withr::with_options(list( "documentation::indent" = TRUE), {
        expect_identical( .Rd_indent(x)
                        , Rd( Rd_text( .Rd.default.indent %<<<% "test strings\n")
                            , Rd_text( .Rd.default.indent %<<<% "second line")
                            ))
    })
    withr::with_options(list( "indent" = TRUE), {
        expect_identical( .Rd_indent(x)
                        , Rd( Rd_text( .Rd.default.indent %<<<% "test strings\n")
                            , Rd_text( .Rd.default.indent %<<<% "second line")
                            ))
    })
    withr::with_options(list( "documentation::Rd::indent" = TRUE
                            , "documentation::Rd::indent.with" = "   "), {
        expect_identical( .Rd_indent(x)
                        , Rd( Rd_text( "   test strings\n")
                            , Rd_text( "   second line")
                            ))
    })
    withr::with_options(list( "documentation::Rd::indent" = TRUE
                            , "documentation::Rd::indent.with" = "\t"), {
        expect_warning( .Rd_indent(x)
                      , class= "documentation-warning-guidelines_violation"
                      )
    })
})
#line 332 "R/Fun-toRd.R"
test_that('.Rd_indent', {#@testing
    expect_error(.Rd_indent(c('test strings')))

    x <- Rd_usage( .Rd.code.newline
                 , Rd_rcode('value \\%if\\% proposition\n')
                 , Rd_rcode('value \\%if\\% proposition \\%otherwise\\% alternate\n')
                 )
    val<- .Rd_indent(x=x, indent=TRUE, indent.with = '  ')
    expect_identical( val
                    , Rd_usage( .Rd.code.newline
                              , Rd_rcode('  value \\%if\\% proposition\n')
                              , Rd_rcode('  value \\%if\\% proposition \\%otherwise\\% alternate\n')
                              ))
})
#line 420 "R/Fun-toRd.R"
test_that('.Rd_strwrap', {#@testing
    x <- stringi::stri_rand_lipsum(1)

    expect_identical(.Rd_strwrap(x, wrap.lines=FALSE, wrap.at=72L)
                    , Rd_text(x))
    val <- .Rd_strwrap(x, wrap.lines=TRUE , wrap.at=72L)
    expect_is(val, 'Rd')
    expect_true(all_inherit(val, c('Rd_TEXT', 'Rd_newline')))
    expect_equal( unlist(val[1,])
                , base::strwrap(x, 72L)
                )
    expect_true( all(unlist(val[2,])=='\n'))
    val <- .Rd_strwrap(x, wrap.lines=TRUE, wrap.at=50)
    expect_identical( unlist(val[1,])
                    , base::strwrap(x, 50)
                    )
    withr::with_options(list( "documentation::Rd::wrap.lines" = TRUE
                            , "documentation::Rd::wrap.at"    = 50), {
        expect_identical( unlist(.Rd_strwrap(x)[1,])
                        , base::strwrap(x, 50)
                        )
    })

    x <- c("Lorem ipsum", stringi::stri_rand_lipsum(3, start_lipsum = FALSE))
    x <- collapse(x, '\n\n')

    expect_identical( unlist(.Rd_strwrap(x, wrap.lines=TRUE, wrap.at=72L)[1,])
                    , unname(unlist(base::strwrap(x, 72)))
                    )
    expect_equal(sum(.Rd_strwrap(x, wrap.lines=TRUE, wrap.at=72L) == ''), 3L)

    expect_identical( unlist(.Rd_strwrap("   hello\n\nworld", wrap.lines=TRUE, wrap.at=72L)[1,])
                    , c("hello", "", "world")
                    )
})
#line 467 "R/Fun-toRd.R"
test_that('Rd_canonize', {#@testing
    rd <- Rd_text("a\nb\nc\n")
    expect_is(rd, 'Rd_TEXT')
    expect_true(is.character(rd))
    expect_length(rd, 1)

    val <- Rd_canonize_text(rd)
    expect_is(rd, 'Rd')
    expect_true(Rd_is_all_text(rd))

    rd <- Rd_examples(Rd( .Rd.code.newline
                        , Rd_rcode("x<- rnorm(100)\n")
                        , Rd_rcode("plot(x)\n")))
    expect_identical(Rd_canonize_text(rd), rd)
    expect_identical(Rd_canonize_code(rd), rd)

    Rd_canonize(Rd_canonize_text(rd))

    expect_identical(Rd_canonize_code(Rd_examples(Rd_rcode("\nx<- rnorm(100)\nplot(x)\n"))), rd)

    rd <- Rd(c( "use the \\backslash to escape."
                 , "and '{}' to group."
                 ))
    val <- Rd_canonize_text(rd)
    expect_is_exactly(val, 'Rd')
    expect_is_exactly(val[[1]], 'Rd_TEXT')
    expect_length(val[[1]], 1L)

    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'documentation'))
    txt <- Rd_rm_srcref(txt)
    expect_identical( Rd_canonize_code(rd <- txt[['\\examples']])
                    , txt[['\\examples']]
                    )

    desc <- txt[['\\description']]
    canonical <- Rd_canonize_text(desc)
    expect_identical( as.character(desc)
                    , as.character(canonical)
                    )

    expect_identical(Rd_canonize_text(txt), txt)
    expect_identical(Rd_canonize_code(txt), txt)
    expect_identical(Rd_canonize(txt), txt)

    x <- Rd_text(c("test strings\n", "second line"))
    val <- Rd_canonize(x)
    expected <- Rd(Rd_text("test strings\n"), Rd_text("second line"))
    expect_identical(val, expected)

    expect_identical(Rd_canonize_text(.Rd.newline), .Rd.newline)


    x <- c( Rd_tag("item"), Rd_text(space(1)), Rd_text("content"))
    expect_identical(Rd_canonize_text(x)[[1]], Rd_tag('item'))
})
#line 569 "R/Fun-toRd.R"
test_that('Rd_wrap_line', {#@testing
    # rd <- Rd( "If ", Rd_code('mean'), ' or ', Rd_code("sd")
    #         , " are not specified they assume the default values of "
    #         ,  Rd_code("0"), " and ", Rd_code("1"), ", respectively.")
    # val <- Rd_wrap_line(rd, 72)
    # expected <-
    #     Rd( "If ", Rd_code('mean'), ' or ', Rd_code("sd")
    #       , " are not specified they assume the default"
    #       , .Rd.newline, "values of ",  Rd_code("0"), " and "
    #       , Rd_code("1"), ", respectively.")
    # expect_identical(val, expected)
    #
    # rd <- Rd_canonize(Rd(collapse(stringi::stri_rand_lipsum(3), '\n\n')))
    # val <- Rd_wrap_line(rd)

})
#line 590 "R/Fun-toRd.R"
test_that('toRd,character', {#@testing toRd,character
    expect_identical( toRd(c("\\hello\n", "%world"))
                    , Rd(Rd_text("\\\\hello\n"), Rd_text("\\%world")))
})
#line 596 "R/Fun-toRd.R"
test_that('toRd.NULL', {#@testing
    expect_identical(toRd(NULL), Rd())
})
#line 607 "R/Fun-toRd.R"
test_that('toRd.list', {#@testing
    l <- list('\\hello ', '%world')
    expect_identical( toRd(l)
                    , s( list( Rd_text("\\\\hello ")
                             , Rd_text("\\%world"))
                       , class='Rd')
                    )

    l <- list( first  = Rd("first text")
             , second = Rd(c("second", "text"))
             , third = NULL
             )

    val <- toRd(l)

    expect_is(val, 'Rd')
    expect_is(val[[1]], 'Rd_TEXT')
})
#line 637 "R/Fun-toRd.R"
test_that('toRd.Rd', {#@testing
    obj <- Rd("test")
    expect_identical(toRd(obj), obj)
    expect_error(toRd(cl(TRUE, 'Rd')))

    obj <- Rd("\\rd")
    expect_identical(toRd(obj), obj)
})
#line 658 "R/Fun-toRd.R"
test_that('toRd,author', {#@testing toRd,author
    obj <- list(author = c( person('Andrew', 'Redd'
                                  , email='andrew.redd@hsc.utah.edu')
                          , person('Drew'  , 'Blue')
                          ))
    val <- toRd(obj)
    expect_is_exactly(val, 'Rd')
    expect_length(val, 1L)
    expect_is_exactly(val[[1]], 'Rd_TEXT')
    expect_length(val[[1]], 1L)
    expect_identical( as.character(toRd(obj))
                    , "Andrew Redd \\email{andrew.redd@hsc.utah.edu} and Drew Blue"
                    )
})
#line 672 "R/Fun-toRd.R"
test_that('toRd.person', {#! @testing
    object <-c( person('First' , 'Author', email='me1@email.com')
              , person('Second', 'Author', email='me2@email.com')
              )
    val <- toRd(object)
    expect_is_exactly(val, 'Rd')
    expect_length(val, 1L)
    expect_is_exactly(val[[1L]], 'Rd_TEXT')
    expect_length(val[[1]], 1L)

    expect_equal( as.character(val)
                , 'First Author \\email{me1@email.com}' %<<% 'and' %<<%
                  'Second Author \\email{me2@email.com}'
                  )
    expect_equal( toRd( c( person('Andrew', 'Redd', email='andrew.redd@hsc.utah.edu')
                         , person('Drew'  , 'Blue')
                         ) )
                , s( list( s('Andrew Redd \\email{andrew.redd@hsc.utah.edu}' %<<%
                             'and Drew Blue'
                            , Rd_tag = 'TEXT'
                            , class = c('Rd_TEXT', 'Rd_tag', 'Rd')))
                           , class='Rd')
                )
})
#line 719 "R/Fun-toRd.R"
test_that('toRd,Documentation-Keyword-method', {#! @testing
    obj <- new('Documentation-Keyword', 'utilities')
    val <- toRd(obj)
    expect_is_exactly(val, 'Rd')
    expect_is_exactly(val[[1]], 'Rd_tag')
    expect_true(is_Rd_tag(val[[1]], '\\keyword'))

    obj <- new('Documentation-Keyword', c('utilities', 'character'))
    val <- toRd(obj)
    expect_is_exactly(val, 'Rd')
    expect_length(val, 2)
    expect_is_exactly(val[[1]], 'Rd_tag')
    expect_true(is_Rd_tag(val[[1]], '\\keyword'))
    expect_equal( collapse0(as.character(val))
                , '\\keyword{utilities}\\keyword{character}')
})
#line 743 "R/Fun-toRd.R"
test_that('toRd,FormattedText/Rd-method', {#! @testing
    obj <- FT_Rd(rd <- Rd( Rd_text("A description of ")
                         , Rd_tag('code', Rd_tag('link', Rd_rcode("toRd")))
                         , .Rd.newline
                         ))
    class(rd) <- s('Rd', package='documentation')

    val <- toRd(obj)
    identical(S3Part(obj, strictS3 =TRUE), rd)
    identical(S3Part(obj, strictS3 =TRUE)[[1]], rd[[1]])
    identical(S3Part(obj, strictS3 =TRUE)[[2]], rd[[2]])
    identical(S3Part(obj, strictS3 =TRUE)[[3]], rd[[3]])

    expect_is(obj, 'FormattedText/Rd')
    val <- toRd(obj)
    expect_is_exactly(val, 'Rd')
    expect_identical( val, rd)

    obj <- FT_Rd('Hello world!')
    val <- toRd(obj)
    expect_is_exactly(val, 'Rd')
    attr(class(val), 'package') <- NULL
    expect_equal( val, Rd('Hello world!'))
    expect_false(identical(toRd(obj), obj))
})
#line 781 "R/Fun-toRd.R"
test_that('toRd,FormattedText/character-method', {#@testing
    obj <- FormattedText(stringi::stri_rand_lipsum(3))
    as.rd <- toRd(obj)
    expect_length(as.rd, 5 )
    expect_length(as.character(as.rd), 5 )
    expect_is_exactly(as.rd, 'Rd')

    expect_true(all(as.rd[c(2,4)]=='\n'))

    expect_identical(toRd(FT()), Rd())

    wrapped <- toRd(obj, wrap.lines=TRUE, wrap.at=50)
    expect_true(length(as.character(wrapped)) > 5L)
    expect_identical(wrapped, Rd_canonize(wrapped))
})
