#' @include Classes.R
#' @include Fun-toRd.R


setMethod('toRd', 'arg-Documentation',
function(obj, ...){
    #Question: does thise need to include more details like default value?
    sprintf("\\item{%s}{%s}", as.character(obj@name), obj@description)
})
if(FALSE){#! @testing
    a <- new("arg-Documentation", name= 'testing', description='a testing argument', default=new('Documentation-No-Default-Value'))
    expect_identical( unclass(toRd(a))
                    , "\\item{testing}{a testing argument}")
}


setMethod('toRd', 'ArgumentList',
function( obj
        , ...
        ){
    formatted.args <- Rd(lapply(obj, toRd, ...))
    Rd_tag(formatted.args, 'arguments', ...)
})
if(FALSE){#! @testing
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
}

