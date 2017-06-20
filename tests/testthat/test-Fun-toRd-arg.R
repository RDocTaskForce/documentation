#! This file was automatically produced by documentation::extract_tests on  2017-06-20 13:15:39
#! changes will be overwritten.
context('tests extracted from file `/mnt/data/projects/rdtf/documentation/R/Fun-toRd-arg.R`')
#line 10 "/mnt/data/projects/rdtf/documentation/R/Fun-toRd-arg.R"
test_that('toRd.arg-Documentation', {#! @testing
    a <- new("arg-Documentation", name= 'testing', description='a testing argument', default=new('Documentation-No-Default-Value'))
    expect_identical(toRd(a), "\\item{testing}{a testing argument}")
})
#line 33 "/mnt/data/projects/rdtf/documentation/R/Fun-toRd-arg.R"
test_that('toRd.ArgumentList', {#! @testing
    a <- new("arg-Documentation", name= 'testing', description='a testing argument', default=new('Documentation-No-Default-Value'))
    b <- arg_('testing', 'a testing argument')
    
    obj <- 
        ArgumentList( arg_('x', 'an argument')
                    , arg_('y', 'another argument')
                    )

    expect_equal( toRd(obj, indent = FALSE, collapse.lines=FALSE)
                , c( '\\arguments{'
                   , '\\item{x}{an argument}'
                   , '\\item{y}{another argument}'
                   , '}'
                   )
                )
    expect_equal( toRd(obj, indent = FALSE, collapse.lines=FALSE)
                , c( '\\arguments{'
                   , '\\item{x}{an argument}'
                   , '\\item{y}{another argument}'
                   , '}'
                   )
                )
    expect_equal( toRd(obj, indent = TRUE, indent.with='    '
                                  , collapse.lines=FALSE)
                , c( '\\arguments{'
                   , '    \\item{x}{an argument}'
                   , '    \\item{y}{another argument}'
                   , '}'
                   )
                )
    expect_equal( toRd(obj, indent = TRUE, indent.with='\t'
                                  , collapse.lines=FALSE)
                , c( '\\arguments{'
                   , '\t\\item{x}{an argument}'
                   , '\t\\item{y}{another argument}'
                   , '}'
                   )
                )
    expect_equal( toRd(obj, indent = FALSE, indent.with='    '
                           , collapse.lines=TRUE, collapse.with='\n'
                           )
                , paste( '\\arguments{'
                       , '\\item{x}{an argument}'
                       , '\\item{y}{another argument}'
                       , '}'
                       , sep='\n')
                )
    expect_equal( toRd(obj, indent = TRUE, indent.with='    '
                           , collapse.lines=TRUE, collapse.with='\n'
                           )
                , paste( '\\arguments{'
                       , '    \\item{x}{an argument}'
                       , '    \\item{y}{another argument}'
                       , '}'
                       , sep='\n')
                )
})
