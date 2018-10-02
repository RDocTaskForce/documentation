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

setMethod('toRd', 'usage/S3method',
function(obj, ...){
    ex <- S3Part(obj, TRUE)[[1]]
    ex[[1L]] <- as.name('.')

    content <- deparse(ex)
    content <- substring(content, 2)

    content <- clean_Rd(content)
    content <- Rd_rcode(content)

    content=Rd_tag('S3method', Rd(toRd(obj@generic)), Rd(toRd(obj@signature)), content)

    Rd_usage(content)
})
if(FALSE){#@testing
    ex <- expression(html_to_Rd.em(html, ...))
    obj <- new('usage/S3method', ex, generic = 'html_to_Rd', signature = 'em')
    rd <- toRd(obj)
    expect_is_exactly(rd, 'Rd')
    expect_true(is_Rd_tag(rd[[1]], '\\usage'))
    expect_true(is_Rd_tag(rd[[c(1,1)]], '\\S3method'))
    expect_identical(collapse0(rd), "\\usage{\\S3method{html_to_Rd}{em}(html, ...)}")
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
    content <- Rd_tag('S4method', Rd(gen), Rd(sig), content)

    Rd_usage(content)
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
    expect_is_exactly(rd, 'Rd')
    expect_true(is_Rd_tag(rd[[1]], '\\usage'))
    expect_true(is_Rd_tag(rd[[c(1,1)]], '\\S4method'))
    expect_identical( collapse0(rd)
                    , "\\usage{\\S4method{plot}{profile.mle,missing}" %<<<%
                      "(x, levels, conf = c(99, 95, 90, 80, 50)/100" %<<<%
                      ", nseg = 50, absVal = TRUE, ...)}"
                    )
}
