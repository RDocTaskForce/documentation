#' @include Classes.R

#' @export
setMethod('toRd', 'usage',
function(obj, ...){
    content <- lapply(obj, deparse)
    content <- lapply(content, clean_Rd)
    content <- lapply(content, Rd_rcode)
    if (length(content)>1) content <- Rd_lines(content)
    Rd_tag("\\usage", content=content, ...)
})
if(FALSE){#@testing
    obj <- as(expression(function_documentation(name, arguments, usage, ...)), 'usage')
    expect_identical( toRd(obj)
                    , Rd_usage(Rd_rcode("function_documentation(name, arguments, usage, ...)"))
                    )

    obj <- as(expression( value %if% proposition
                        , value %if% proposition %otherwise% alternate
                        ), 'usage')
    rd <- toRd(obj)
    expect_is_exactly(rd, 'Rd_tag')
    expect_true(is_Rd_tag(rd, '\\usage'))
    expect_identical( rd
                    , Rd_tag( "\\usage"
                            , Rd_rcode('\n')
                            , Rd_rcode('value \\%if\\% proposition\n')
                            , Rd_rcode('value \\%if\\% proposition \\%otherwise\\% alternate\n')
                            ))

    expect_identical( toRd(obj, indent=TRUE, indent.with='    ')
                    , Rd_usage( '    value \\%if\\% proposition'
                              , '    value \\%if\\% proposition \\%otherwise\\% alternate'
                              ))
}

setMethod('toRd', 'usage/S3method',
function(obj, ...){
    ex <- S3Part(obj, TRUE)[[1]]
    ex[[1L]] <- as.name('.')

    content <- deparse(ex)
    content <- substring(content, 2)

    content <- clean_Rd(content)
    content <- Rd_rcode(content)

    tag <- Rd_tag('\\S3method', Rd(toRd(obj@generic)), Rd(toRd(obj@signature)))
    Rd_tag("\\usage", content=list(tag, content))
})
if(FALSE){#@testing
    ex <- expression(html_to_Rd.em(html, ...))
    obj <- new('usage/S3method', ex, generic = 'html_to_Rd', signature = 'em')
    rd <- toRd(obj)
    expect_is_exactly(rd, 'Rd_tag')
    expect_true(is_Rd_tag(rd, '\\usage'))
    expect_true(is_Rd_tag(rd[[1]], '\\S3method'))
    expect_identical(format(rd), "\\usage{\\S3method{html_to_Rd}{em}(html, ...)}")
}

setMethod('toRd', 'usage/S4method',
function(obj, ...){
    ex <- S3Part(obj, TRUE)[[1]]
    ex[[1L]] <- as.name('.')

    content <- deparse(ex, 500)
    content <- substring(content, 2)

    content <- clean_Rd(content)
    content <- Rd_rcode(content)

    sig <- Rd_rcode(collapse(obj@signature, ','))
    gen <- toRd(obj@generic)
    tag <- Rd_tag('\\S4method', content = list(Rd(gen), Rd(sig)))

                      # , content), .check = FALSE)
    Rd_tag('\\usage', content = list(tag,  content))
})
if(FALSE){#@testing
    # Taken from stat4 package file src/library/stats4/man/plot-methods.Rd
    #
    # \S4method{plot}{profile.mle,missing}(x, levels
    #   , conf = c(99, 95, 90, 80, 50)/100, nseg = 50
    #   , absVal = TRUE, \dots)
    ex <- expression(plot(x, levels, conf = c(99, 95, 90, 80, 50)/100, nseg = 50, absVal = TRUE, ...))
    obj <- new('usage/S4method', ex, generic = 'plot'
              , signature = signature(x='profile.mle', y='missing')
              )
    rd <- toRd(obj)
    expect_is_exactly(rd, 'Rd_tag')
    expect_true(is_Rd_tag(rd, '\\usage'))
    expect_true(is_Rd_tag(rd[[1]], '\\S4method'))
    expect_identical( format(rd)
                    , "\\usage{\\S4method{plot}{profile.mle,missing}" %<<<%
                      "(x, levels, conf = c(99, 95, 90, 80, 50)/100" %<<<%
                      ", nseg = 50, absVal = TRUE, ...)}"
                    )
}
