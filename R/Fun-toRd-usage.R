#' @include Classes.R
#' @include Fun-toRd.R

setMethod('toRd', 'usage',
function(obj, ...){
    content <- lapply(obj, deparse)
    content <- lapply(content, clean_Rd)
    content <- lapply(content, Rd_code)
    if (length(content)>1L) {
        content <- rbind( c(list(NULL), content), .Rd.code.newline)[-1]
        content <- .Rd_indent(cl(content, 'Rd'), ...)
    }
    Rd_usage(content=purrr::compact(content))
})
if(FALSE){#@testing
    obj <- as(expression(function_documentation(name, arguments, usage, ...)), 'usage')
    expect_identical( toRd(obj)
                    , Rd(Rd_usage(Rd_code("function_documentation(name, arguments, usage, ...)")))
                    )

    obj <- as(expression( value %if% proposition
                        , value %if% proposition %otherwise% alternate
                        ), 'usage')
    rd <- toRd(obj)
    expect_identical( rd
                    , Rd(Rd_usage( .Rd.code.newline
                                 , Rd_code('value \\%if\\% proposition'), .Rd.code.newline
                                 , Rd_code('value \\%if\\% proposition \\%otherwise\\% alternate'), .Rd.code.newline
                                 )))
    expect_identical( toRd(obj, indent=TRUE, indent.with=.Rd.default.indent)
                    , Rd(Rd_usage( .Rd.code.newline
                                 , .Rd.default.indent, Rd_code('value \\%if\\% proposition'), .Rd.code.newline
                                 , .Rd.default.indent, Rd_code('value \\%if\\% proposition \\%otherwise\\% alternate'), .Rd.code.newline
                                 )))
}

