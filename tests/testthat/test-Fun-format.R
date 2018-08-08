#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Fun-format.R`')
#line 68 "/rdtf/documentation/R/Fun-format.R"
test_that('get_formatter', {#@testing
    expect_identical(get_formatter('toRd'), toRd)
    expect_identical(get_formatter('Rd'), toRd)

    expect_error( get_formatter('not a format')
                , class = "documentation-error-format-not_defined"
                )
})
#line 87 "/rdtf/documentation/R/Fun-format.R"
test_that('get_formatter_ext', {#@testing
    expect_identical(get_formatter_ext('toRd'), '.Rd')
    expect_identical(get_formatter_ext('Rd'), '.Rd')

    expect_warning( value <- get_formatter_ext('not_a_format')
                  , class = "documentation-error-format-not_defined"
                  )
    expect_identical( value, '.not_a_format')
})
#line 106 "/rdtf/documentation/R/Fun-format.R"
test_that('get_formatter_dir', {#@testing
    expect_identical(get_formatter_dir('toRd'), 'man')
    expect_identical(get_formatter_dir('Rd'), 'man')

    expect_warning( value <- get_formatter_dir('not_a_format')
                  , class = "documentation-error-format-not_defined"
                  )
    expect_identical(value, '.')
})
