#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `util-testing.R`')
#line 25 "R/util-testing.R"
test_that('expect_valid', {#@testing
    bad <- s(1L, class='Rd', Rd_tag='integer')
    expect_error( expect_valid(bad)
                , "`bad` is not valid;" %<<% dQuote("object is not a list")
                )

    good <- s(list(), class='Rd')
    expect_valid(good)
})
#line 119 "R/util-testing.R"
test_that('are', {#@testing
    lst <- list('a', 1L, TRUE)

    expect_true(all(are(lst, 'ANY')))
    expect_identical(are(lst, 'character'), c(T,F,F))
    expect_identical(are(lst, 'integer'), c(F,T,F))
    expect_identical(are(lst, 'numeric'), c(F,T,F))
})
#line 149 "R/util-testing.R"
test_that('all_are_exactly', {#@testing
    l <- list( 'a', 'b', 'c'
             , 1, 2)
    expect_identical( validate_that(all_are_exactly(l, 'character'))
                    , "`l` has bad elements at positions 4 and 5" %<<%
                      "which are not of class" %<<%
                      dQuote("character") %<<<% '.')
    expect_identical( validate_that(all_are_exactly(list(1,2), 'integer', '...'))
                    , "... has bad elements at positions 1 and 2" %<<%
                      "which are not of class" %<<%
                      dQuote("integer") %<<<% '.')
    expect_identical( validate_that(all_are_exactly(list(1L,2L), 'numeric', '...'))
                    , "... has bad elements at positions 1 and 2" %<<%
                      "which are not of class" %<<%
                      dQuote("numeric") %<<<% '.')
    expect_identical( validate_that(all_are_exactly(list(1, 2L), 'numeric', '...'))
                    , "... has bad element at position 2" %<<%
                      "which is not of class" %<<%
                      dQuote("numeric") %<<<% '.')
    expect_true(all_are_exactly(list(1L, 2L), 'integer'))
})
#line 197 "R/util-testing.R"
test_that('expect_is_exactly', {#@testing
    x <- list(1:3)

    expect_identical(expect_is_exactly(x, 'list'), x)

    class(x) <- c('class', 'super1', 'super2')

    expect_is_exactly(x, 'class')

    expect_is(x, 'super1')
    expect_error( expect_is_exactly(x, 'super1')
                , "`x` is a class/super1/super2; should be exactly a `super1`."
                )
})
#line 228 "R/util-testing.R"
test_that('is_valid_regex', {#@testing
    expect_true(is_valid_regex("^hello world$"))
    expect_false(is_valid_regex("^hello (world$"))
    expect_identical( validate_that(is_valid_regex("^hello (world$"))
                    , "invalid regular expression " %<<<%
                      "'^hello (world$', reason 'Missing ')''"
                    )
})
#line 249 "R/util-testing.R"
test_that('is_nonempty_string', {#@testing
    expect_true(is_nonempty_string("hello world"))
    expect_false(is_nonempty_string(c("hello", "world")))
    expect_false(is_nonempty_string(character(0)))
    expect_false(is_nonempty_string(NA_character_))
    expect_false(is_nonempty_string(''))

    expect_identical( validate_that(is_nonempty_string(character(0)))
                    , sQuote("character(0)") %<<% .nonempty.string.msg)

    bad <- NA
    expect_identical( validate_that(is_nonempty_string(bad))
                    , sQuote("bad") %<<%.nonempty.string.msg)
})
#line 276 "R/util-testing.R"
test_that('is_optional_string', {#@testing
    expect_true(is_optional_string("hello"))
    expect_true(is_optional_string(character(0)))
    expect_false(is_optional_string(NA_character_))
    expect_false(is_optional_string(''))
    expect_false(is_optional_string(letters))

    bad <- NA_character_
    expect_identical(validate_that(is_optional_string(bad))
                    , sQuote('bad') %<<% .optional.string.msg)
})
#line 312 "R/util-testing.R"
test_that('new_namespace_env', {#@testing
    ns <- new_namespace_env("test package environment")
    expect_true(isNamespace(ns))
    expect_false(isNamespaceLoaded("test package environment"))
})
#line 331 "R/util-testing.R"
test_that('new_pkg_test_environment', {#@testing
    ns <- new_pkg_test_environment()
    expect_true(isNamespace(ns))
    expect_equal(getPackageName(ns), "test package environment")
    expect_equal(environmentName(ns), "test package environment")
})
