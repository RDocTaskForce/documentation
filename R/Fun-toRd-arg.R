#' @include Classes.R
#' @include Fun-toRd.R


setMethod('toRd', 'arg-Documentation',
function(obj, ...){
    #Question: does thise need to include more details like default value?
    sprintf("\\item{%s}{%s}", as.character(obj@name), obj@description)
})
if(FALSE){#! @testing
    a <- new("arg-Documentation", name= 'testing', description='a testing argument', default=new('Documentation-No-Default-Value'))
    expect_identical(toRd(a), Rd("\\item{testing}{a testing argument}"))
}



setMethod('toRd', 'ArgumentList',
function( obj
        , ...
        , indent          = default(indent        , FALSE )
        , indent.with     = default(indent.with   , '    ')
        , collapse.lines  = default(collapse.lines, FALSE )
        , collapse.with   = default(collapse.with , '\n'  )
        ){
    formatted.args <- sapply(obj, toRd, ...)
    if(indent)
        formatted.args <- paste0(indent.with, formatted.args)
    lines <- Rd_tag(formatted.args, 'arguments')
    if(collapse.lines)
        lines <- paste(lines, collapse = collapse.with)
    return(lines)
})
if(FALSE){#! @testing
    a <- new("arg-Documentation", name= 'testing', description='a testing argument', default=new('Documentation-No-Default-Value'))
    b <- arg_('testing', 'a testing argument')

    obj <-
        ArgumentList( arg_('x', 'an argument')
                    , arg_('y', 'another argument')
                    )

    expect_equal( toRd(obj, indent = FALSE, collapse.lines=FALSE)
                , Rd(c( '\\arguments{'
                      , '\\item{x}{an argument}'
                      , '\\item{y}{another argument}'
                      , '}'
                      ))
                )
    expect_equal( toRd(obj, indent = FALSE, collapse.lines=FALSE)
                , Rd(c( '\\arguments{'
                      , '\\item{x}{an argument}'
                      , '\\item{y}{another argument}'
                      , '}'
                      ))
                )
    expect_equal( toRd(obj, indent = TRUE, indent.with='    '
                                  , collapse.lines=FALSE)
                , Rd(c( '\\arguments{'
                      , '    \\item{x}{an argument}'
                      , '    \\item{y}{another argument}'
                      , '}'
                      ))
                )
    expect_equal( toRd(obj, indent = TRUE, indent.with='\t'
                                  , collapse.lines=FALSE)
                , Rd(c( '\\arguments{'
                      , '\t\\item{x}{an argument}'
                      , '\t\\item{y}{another argument}'
                      , '}'
                      ))
                )
    expect_equal( toRd(obj, indent = FALSE, indent.with='    '
                           , collapse.lines=TRUE, collapse.with='\n'
                           )
                , Rd(paste( '\\arguments{'
                          , '\\item{x}{an argument}'
                          , '\\item{y}{another argument}'
                          , '}'
                          , sep='\n'))
                )
    expect_equal( toRd(obj, indent = TRUE, indent.with='    '
                           , collapse.lines=TRUE, collapse.with='\n'
                           )
                , Rd(paste( '\\arguments{'
                          , '    \\item{x}{an argument}'
                          , '    \\item{y}{another argument}'
                          , '}'
                          , sep='\n'))
                )
}
