#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Fun-format.R`')
#line 68 "R/Fun-format.R"
test_that('get_formatter', {#@testing
    formatter.toRd <- get_formatter('toRd')
    expect_is(formatter.toRd, 'nonstandardGenericFunction')
    expect_identical( formatter.toRd@generic
                    , s('toRd', package="documentation"))

    formatter.Rd <- get_formatter('toRd')
    expect_is(formatter.Rd, 'nonstandardGenericFunction')
    expect_identical( formatter.Rd@generic
                    , s('toRd', package="documentation"))

    expect_identical(formatter.Rd, formatter.toRd)

    expect_error( get_formatter('not a format')
                , class = "documentation-error-format-not_defined"
                )
})
#line 96 "R/Fun-format.R"
test_that('get_formatter_ext', {#@testing
    expect_identical(get_formatter_ext('toRd'), '.Rd')
    expect_identical(get_formatter_ext('Rd'), '.Rd')

    expect_warning( value <- get_formatter_ext('not_a_format')
                  , class = "documentation-error-format-not_defined"
                  )
    expect_identical( value, '.not_a_format')
})
#line 115 "R/Fun-format.R"
test_that('get_formatter_dir', {#@testing
    expect_identical(get_formatter_dir('toRd'), 'man')
    expect_identical(get_formatter_dir('Rd'), 'man')

    expect_warning( value <- get_formatter_dir('not_a_format')
                  , class = "documentation-error-format-not_defined"
                  )
    expect_identical(value, '.')
})
