#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Fun-toRd-example.R`')
#line 18 "R/Fun-toRd-example.R"
test_that('trimws_example', {#@testing
    txt <- c( ""
            , "    # prints hello world."
            , "    hw()"
            , "    "
            , ""
            , "    {# bracketed code"
            , "        do_something()"
            , "    }"
            , "    "
            )
    expect_equal( trimws_example(txt)
                , c( "# prints hello world."
                   , "hw()"
                   , ""
                   , "{# bracketed code"
                   , "    do_something()"
                   , "}"
                   )
                )
})
#line 72 "R/Fun-toRd-example.R"
test_that('doc_example_get_src_lines', {#@testing
    txt <-{""                                                             %\%
           "#' An example character vector"                               %\%
           "#'"                                                           %\%
           "#' This example is used to test extracting documentation from"%\%
           "#' a character vector which typically does not contain"       %\%
           "#' a source reference."                                       %\%
           "example_character <-"                                         %\%
           "    c( \"first\"  #< the first element"                       %\%
           "     , \"second\" #< the second element"                      %\%
           "     )"                                                       %\%
           "" %\%
           "# Comment before" %\%
           "  do_something()" %\%
           "# comment after"  %\%
           ""}
    p <- parse(text=txt, keep.source = TRUE)


    expect_equal( doc_example_get_src_lines(getSrcref(p)[[1]])
                , c( "#' An example character vector"
                   , "#'"
                   , "#' This example is used to test extracting documentation from"
                   , "#' a character vector which typically does not contain"
                   , "#' a source reference."
                   , "example_character <-"
                   , "    c( \"first\"  #< the first element"
                   , "     , \"second\" #< the second element"
                   , "     )"
                   ))

    src <- getSrcref(p)[[2]]
    expect_equal( doc_example_get_src_lines(getSrcref(p)[[2]])
                , c( "# Comment before"
                   , "  do_something()"
                   , "# comment after"
                   ))
    expect_equal( doc_example_get_src_lines( getSrcref(p) )
                , c( "#' An example character vector"
                   , "#'"
                   , "#' This example is used to test extracting documentation from"
                   , "#' a character vector which typically does not contain"
                   , "#' a source reference."
                   , "example_character <-"
                   , "    c( \"first\"  #< the first element"
                   , "     , \"second\" #< the second element"
                   , "     )"
                   , ""
                   , "# Comment before"
                   , "  do_something()"
                   , "# comment after"
                   ))

})
#line 148 "R/Fun-toRd-example.R"
test_that('toRd,example-method', {#@testing
    simple.text <- "
    # prints hello world.
    hw()
    "
    p <- parse(text=simple.text, keep.source=TRUE)
    ex <- new('example', p)

    expect_equal( toRd(ex, use.source=TRUE)
                , Rd( Rd_rcode( "# prints hello world.\n")
                    , Rd_rcode( "hw()\n")
                    ))

    ex2 <- new('example', expression(test(x,y)))
    expect_is(ex2, 'Documentation-example')
    expect_null(getSrcref(ex2))
    expect_identical(toRd(ex2), Rd(Rd_rcode(c("test(x, y)\n"))))

    ex3 <- new('example', expression( a <- "test"
                                    , b <- Rd(a)
                                    , expect_is(b, 'Rd')
                                    ))
    expect_equal( toRd(ex3)
                , Rd(Rd_rcode( 'a <- "test"\n')
                    ,Rd_rcode( 'b <- Rd(a)\n')
                    ,Rd_rcode( 'expect_is(b, "Rd")\n')
                    ))
})
#line 176 "R/Fun-toRd-example.R"
test_that('toRd,example-method', {#@testing
    ex.blank <- new('example')
    expect_identical(toRd(ex.blank), Rd(character(0)))


    ex.file <- system.file("examples", "example_character.R", package='documentation')
    obj <- new('example', parse(ex.file, keep.source=TRUE))

    lines <- readLines(ex.file)
    expect_identical( toRd(obj)
                    , Rd_canonize(Rd_rcode(paste0(lines[nchar(lines)>0L], '\n')))
                    )

    whole.src <- attr(obj, 'wholeSrcref')
    attr(obj, 'wholeSrcref') <- NULL
    expect_true( is.null(src <- attr(obj, 'wholeSrcref')))
    expect_true(!is.null(src <- attr(obj, 'srcref')))
    expect_identical( toRd(obj)
                    , Rd_canonize(Rd_rcode(paste0(lines[nchar(lines)>0L], '\n')))
                    )
})
#line 209 "R/Fun-toRd-example.R"
test_that('toRd,Documentation-Examples-method', {#@testing
    simple.text <- "
    # prints hello world.
    hw()
    "
    p <- parse(text=simple.text, keep.source=TRUE)
    ex <- new('example', p)
    ex2 <- new('example', expression(test(x,y)))
    ex3 <- new('example', expression( a <- "test"
                                    , b <- Rd(a)
                                    , expect_is(b, 'Rd')
                                    ))
    examples <- new('Documentation-Examples', list(ex, ex2, ex3))

    rd <- toRd(examples)
    expect_is_exactly(rd, 'Rd')
    expect_true(is_Rd_tag(rd[[1]], "\\examples"))
    expect_identical( collapse0(as.character(rd))
                    , "\\examples{" %\%
                      "# prints hello world." %\%
                      "hw()" %\%
                      '' %\%
                      "test(x, y)" %\%
                      '' %\%
                      'a <- "test"' %\%
                      'b <- Rd(a)' %\%
                      'expect_is(b, "Rd")' %\%
                      "}")

    rd <- toRd(examples, indent=TRUE, indent.with=space(4))
    expect_is_exactly(rd, 'Rd')
    expect_true(is_Rd_tag(rd[[1]], "\\examples"))
    expect_identical( as.character(collapse0(rd))
                    , "\\examples{" %\%
                      "    # prints hello world." %\%
                      "    hw()" %\%
                      '' %\%
                      "    test(x, y)" %\%
                      '' %\%
                      '    a <- "test"' %\%
                      '    b <- Rd(a)' %\%
                      '    expect_is(b, "Rd")' %\%
                      "}"
                    )

    ex.blank <- new('Documentation-Examples')
    expect_identical(toRd(ex.blank), Rd())
})
