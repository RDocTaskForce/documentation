#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Fun-toRd-arg.R`')
#line 12 "R/Fun-toRd-arg.R"
test_that('toRd,arg-Documentation-method', {#! @testing
    obj <- new("arg-Documentation", name= 'testing'
              , description='a testing argument'
              , default=new('Documentation-No-Default-Value'))
    expect_equal( toRd(obj), Rd(Rd_item("testing", "a testing argument")))
})
#line 28 "R/Fun-toRd-arg.R"
test_that('toRd,ArgumentList-method', {#! @testing
    a <- new("arg-Documentation", name= 'testing', description='a testing argument', default=new('Documentation-No-Default-Value'))
    b <- arg_('testing', 'a testing argument')

    obj <-
        ArgumentList( arg_('x', 'an argument')
                    , arg_('y', 'another argument')
                    )

    expect_equal( toRd(obj)
                , Rd(Rd_arguments( Rd_item("x", "an argument")
                                 , Rd_item("y", "another argument")
                                 , indent=FALSE)))
    expect_equal( toRd(obj, indent = TRUE)
                , Rd(Rd_arguments( Rd_item("x", "an argument")
                                 , Rd_item("y", "another argument"), indent=TRUE ))
                )
    expect_equal( suppressWarnings(collapse0(as.character(toRd( obj, indent = TRUE, indent.with = '\t'))))
                , '\\arguments{' %\%
                  '\t\\item{x}{an argument}' %\%
                  '\t\\item{y}{another argument}' %\%
                  '}'
                )

    expect_equal( toRd(obj, indent = FALSE, indent.with='    '
                      ) %>% as.character %>% collapse0
                , '\\arguments{' %\%
                  '\\item{x}{an argument}' %\%
                  '\\item{y}{another argument}' %\%
                  '}'
                )
    expect_equal( toRd(obj, indent = TRUE, indent.with='    ') %>% as.character %>% collapse0
                , '\\arguments{' %\%
                  '    \\item{x}{an argument}' %\%
                  '    \\item{y}{another argument}' %\%
                  '}'
                )
})
