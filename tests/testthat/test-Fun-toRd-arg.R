#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Fun-toRd-arg.R`')
#line 10 "R/Fun-toRd-arg.R"
test_that('toRd,arg-Documentation-method', {#! @testing
    a <- new("arg-Documentation", name= 'testing', description='a testing argument', default=new('Documentation-No-Default-Value'))
    expect_identical( unclass(toRd(a))
                    , "\\item{testing}{a testing argument}")
})
#line 24 "R/Fun-toRd-arg.R"
test_that('toRd,ArgumentList-method', {#! @testing
    a <- new("arg-Documentation", name= 'testing', description='a testing argument', default=new('Documentation-No-Default-Value'))
    b <- arg_('testing', 'a testing argument')

    obj <-
        ArgumentList( arg_('x', 'an argument')
                    , arg_('y', 'another argument')
                    )

    expect_equal( unclass(toRd(obj))
                , c( '\\arguments{'
                   , '\\item{x}{an argument}'
                   , '\\item{y}{another argument}'
                   , '}'
                   )
                )
    expect_equal( unclass(toRd(obj, indent = TRUE, collapse.lines=FALSE))
                , c( '\\arguments{'
                   , '  \\item{x}{an argument}'
                   , '  \\item{y}{another argument}'
                   , '}'
                   )
                )
    expect_equal( unclass(toRd( obj, indent = TRUE
                              , indent.with='    '
                              ))
                , c( '\\arguments{'
                   , '    \\item{x}{an argument}'
                   , '    \\item{y}{another argument}'
                   , '}'
                   )
                )
    expect_warning(val <- toRd(obj, indent = TRUE, indent.with='\t'
                                  , collapse.lines=FALSE))
    expect_equal( unclass(val)
                , c( '\\arguments{'
                   , '\t\\item{x}{an argument}'
                   , '\t\\item{y}{another argument}'
                   , '}'
                   )
                )

    expect_equal( unclass(toRd(obj, indent = FALSE, indent.with='    '
                              , collapse.lines=TRUE, collapse.with='\n'
                              ))
                , paste( '\\arguments{'
                       , '\\item{x}{an argument}'
                       , '\\item{y}{another argument}'
                       , '}'
                       , sep='\n')
                )
    expect_equal( unclass(toRd(obj, indent = TRUE, indent.with='    '
                              , collapse.lines=TRUE, collapse.with='\n'
                              ))
                , paste( '\\arguments{'
                       , '    \\item{x}{an argument}'
                       , '    \\item{y}{another argument}'
                       , '}'
                       , sep='\n')
                )
})
