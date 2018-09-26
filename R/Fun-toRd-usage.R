#' @include Classes.R
#' @include Fun-toRd.R

#' @export
setMethod('toRd', 'usage',
function(obj, ...){
    content <- lapply(obj, deparse)
    content <- lapply(content, clean_Rd)
    content <- lapply(content, Rd_rcode)
    if (length(content)>1L) {
        content <- c(.Rd.code.newline, Rd_lines(content))
        content <- Rd_canonize(cl(content, 'Rd'), ...)
    }
    Rd_usage(content=content)
})
if(FALSE){#@testing
    obj <- as(expression(function_documentation(name, arguments, usage, ...)), 'usage')
    expect_identical( toRd(obj)
                    , Rd(Rd_usage(Rd_rcode("function_documentation(name, arguments, usage, ...)")))
                    )

    obj <- as(expression( value %if% proposition
                        , value %if% proposition %otherwise% alternate
                        ), 'usage')
    rd <- toRd(obj)
    expect_is_exactly(rd, 'Rd')
    expect_true(is_Rd_tag(rd[[1]], '\\usage'))
    expect_identical( rd
                    , Rd(Rd_usage( .Rd.code.newline
                                 , Rd_rcode('value \\%if\\% proposition\n')
                                 , Rd_rcode('value \\%if\\% proposition \\%otherwise\\% alternate\n')
                                 )))

    expect_identical( toRd(obj, indent=TRUE, indent.with=.Rd.default.indent)
                    , Rd(Rd_usage( .Rd.code.newline
                                 , Rd_rcode('    value \\%if\\% proposition\n')
                                 , Rd_rcode('    value \\%if\\% proposition \\%otherwise\\% alternate\n')
                                 )))
}

