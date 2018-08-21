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

    assign('my_formatter', function(obj, ...)html_to_Rd(obj, ...)
          , envir = globalenv())

    expect_warning( fun <- #withr::with_environment(globalenv(), {
                        get_formatter("my_formatter")
                    #})
                  , class='documentation-warning-format-not_defined'
                  )
    expect_identical(fun, globalenv()$my_formatter)
    rm(list='my_formatter', envir = globalenv())
})
#line 107 "R/Fun-format.R"
test_that('get_formatter_ext', {#@testing
    expect_identical(get_formatter_ext('toRd'), '.Rd')
    expect_identical(get_formatter_ext('Rd'), '.Rd')

    expect_warning( value <- get_formatter_ext('not_a_format')
                  , class = "documentation-error-format-not_defined"
                  )
    expect_identical( value, '.not_a_format')
})
#line 126 "R/Fun-format.R"
test_that('get_formatter_dir', {#@testing
    expect_identical(get_formatter_dir('toRd'), 'man')
    expect_identical(get_formatter_dir('Rd'), 'man')

    expect_warning( value <- get_formatter_dir('not_a_format')
                  , class = "documentation-error-format-not_defined"
                  )
    expect_identical(value, '.')
})
#line 156 "R/Fun-format.R"
test_that('format.Documentation', {#@testing
    docs <- function_documentation('test', arguments = arg_('x', 'argument'))
    rd <- format(docs, fmt='Rd')
    expect_identical( rd
                    , c( '\\name{test}'
                       , '\\arguments{\\item{x}{argument}}'
                       , '\\usage{test(x)}'
                       ))
})
