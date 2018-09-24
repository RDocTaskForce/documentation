#' @include Classes.R
#' @include Fun-toRd.R


### toRd,arg-Documentation #####
setMethod('toRd', 'arg-Documentation',
function(obj, ...){
    #Question: does thise need to include more details like default value?
    Rd_item(doc_get_name(obj), toRd(doc_get_description(obj)))
})
if(FALSE){#! @testing
    obj <- new("arg-Documentation", name= 'testing'
              , description='a testing argument'
              , default=new('Documentation-No-Default-Value'))
    expect_equal( toRd(obj), Rd(Rd_item("testing", "a testing argument")))
}

### toRd,ArgumentList #####
setMethod('toRd', 'ArgumentList',
function( obj
        , ...
        ){
    formatted.args <- compact_Rd(lapply(obj, toRd, ...))
    Rd_tag(tag='arguments', content=Rd_lines(formatted.args), opt=NULL, ...)
})
if(FALSE){#! @testing
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
}

