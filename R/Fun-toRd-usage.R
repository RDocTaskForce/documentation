#' @include Classes.R
#' @include Fun-toRd.R

setMethod('toRd', 'usage', 
function(obj, ...){
    Rd_tag(cl(clean_Rd(unlist(lapply(obj, deparse))), 'Rd'), 'usage')
})
if(FALSE){#@testing
    obj <- as(expression(function_documentation(name, arguments, usage, ...)), 'usage')
    expect_identical(toRd(obj), cl(Rd("\\usage{function_documentation(name, arguments, usage, ...)}"), "Rd_tag"))
    
    obj <- as(expression( value %if% proposition
                        , value %if% proposition %otherwise% alternate
                        ), 'usage')
    expect_identical(toRd(obj)
                    , cl(Rd(c('\\usage{'
                             , 'value \\%if\\% proposition'
                             , 'value \\%if\\% proposition \\%otherwise\\% alternate'
                             , '}')), 'Rd_tag') )
}

