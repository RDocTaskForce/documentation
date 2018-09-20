#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `util-testing.R`')
#line 23 "R/util-testing.R"
test_that('expect_valid', {#@testing
    bad <- s(1L, class='Rd', Rd_tag='integer')
    expect_error( expect_valid(bad)
                , "`bad` is not valid;" %<<% dQuote("object is not a list")
                )

    good <- s(list(), class='Rd')
    expect_valid(good)
})
#line 134 "R/util-testing.R"
test_that('all_are', {#@testing
    l <- list( 'a', 'b', 'c'
             , 1, 2)
    expect_identical( validate_that(all_are(l, 'character'))
                    , "`l` has bad elements at positions 4 and 5" %<<%
                      "which are not of class" %<<%
                      dQuote("character") %<<<% '.')
    expect_identical( validate_that(all_are(list(1,2), 'integer', '...'))
                    , "... has bad elements at positions 1 and 2" %<<%
                      "which are not of class" %<<%
                      dQuote("integer") %<<<% '.')
    expect_identical( validate_that(all_are(list(1L,2L), 'numeric', '...'))
                    , "... has bad elements at positions 1 and 2" %<<%
                      "which are not of class" %<<%
                      dQuote("numeric") %<<<% '.')
    expect_identical( validate_that(all_are(list(1, 2L), 'numeric', '...'))
                    , "... has bad element at position 2" %<<%
                      "which is not of class" %<<%
                      dQuote("numeric") %<<<% '.')
    expect_true(all_are(list(1L, 2L), 'integer'))
})
