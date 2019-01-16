#' @include Classes.R


### toRd,arg-Documentation #####
#' @export
setMethod('toRd', 'arg-Documentation',
function(obj, ...){
    #Question: does thise need to include more details like default value?
    Rd_item(doc_get_name(obj), toRd(doc_get_description(obj)))
})
if(FALSE){#! @testing
    obj <- new("arg-Documentation", name= 'testing'
              , description='a testing argument'
              , default=new('Documentation-No-Default-Value'))
    expect_equal( toRd(obj), Rd_item("testing", "a testing argument"))
    expect_identical( format(toRd(obj)), "\\item{testing}{a testing argument}")
}

### toRd,ArgumentList #####
#' @export
setMethod('toRd', 'ArgumentList',
function( obj
        , ...
        ){
    formatted.args <- lapply(obj, toRd)
    Rd_arguments( items=formatted.args, ...)
})
if(FALSE){#! @testing
    a <- new("arg-Documentation", name= 'testing', description='a testing argument', default=new('Documentation-No-Default-Value'))
    b <- arg_('testing', 'a testing argument')

    obj <-
        ArgumentList( arg_('x', 'an argument')
                    , arg_('y', 'another argument')
                    )

    expect_equal( toRd(obj, indent=FALSE)
                , Rd_arguments( Rd_item("x", "an argument")
                              , Rd_item("y", "another argument")
                              , indent=FALSE))
    expect_equal( toRd(obj, indent = TRUE)
                , Rd_arguments( Rd_item("x", "an argument")
                              , Rd_item("y", "another argument")
                              , indent=TRUE )
                )
    expect_equal( format(toRd( obj, indent = TRUE, indent.with = '  '))
                , '\\arguments{' %\%
                  '  \\item{x}{an argument}' %\%
                  '  \\item{y}{another argument}' %\%
                  '}'
                )

    expect_equal( format(toRd(obj, indent = FALSE, indent.with='    '))
                , '\\arguments{' %\%
                  '\\item{x}{an argument}' %\%
                  '\\item{y}{another argument}' %\%
                  '}'
                )
    expect_equal( format(toRd(obj, indent = TRUE, indent.with='    '))
                , '\\arguments{' %\%
                  '    \\item{x}{an argument}' %\%
                  '    \\item{y}{another argument}' %\%
                  '}'
                )
}

