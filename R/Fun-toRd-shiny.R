
.html5.tags.discouraged.reasons <-
    .T( b      = "Use strong"
      , center = "Use css"
      , i      = "Use em"
      , small  = "Do not use formatting tags."
      , sup    = "Do not use formatting tags."
      , sub    = "Do not use formatting tags."
      , u      = "Do not use formatting tags."
      )
.html5.tags.discouraged <- names(.html5.tags.discouraged.reasons)
.html5.tags.allowed <-
    .T( a, abbr, address, aside
      , b, blockquote, br
      , caption, cite, code
      , dd, div, dl, dt
      , em
      , figcaption, figure
      , h1, h2, h3, h4, h5, h6
      , hr, html
      , img
      , kbd
      , li
      , ol
      , p, pre, q
      , samp, section, span, strong
      , table, tbody, td, tfoot, th, thead, title, tr
      , ul
      )
.html5.tags.disallowed <-
    .T( area, article, audio
      , base, bdi, bdo, body
      , canvas, col, colgroup, command
      , datalist, del, details, dnf
      , embed
      , fieldset, footer, form
      , head, header, hgroup
      , iframe, input, ins
      , keygen
      , label, legend, link
      , map, mark, menu, meta
      , nav, noscript
      , object, optgroup, option, output
      , param, progress
      , rp, rt, ruby
      , s      #< Used for no text that is longer correct.
      , script, select, source, style, summary
      , textarea, time, track, tt
      , var, video, wbr
      )

html_to_Rd <- function(html, ...){
    if (inherits(html, 'Rd')) return(html)
    if (is.character(html)) return(Rd(html))
    assert_that(inherits(html, 'shiny.tag'))
    if (!is.null(.f <- match.fun(paste0('html_to_Rd.', html$name))))
        return(.f(html, ...))
    if (html$name %in% .html5.tags.allowed){
        doc_error(._("html_to_Rd.%s is not yet implimented.", html$name)
                 , type = "html_to_Rd-not_implimented")
    } else
    if (html$name %in% .html5.tags.disallowed){
        doc_error(._("Cannot convert shiny.tag of type %1$s." %<<%
                     "While %1$s is a valid HTML5 tag, documentation" %<<%
                     "does not currently support it's conversion to Rd."
                    , sQuote(html$name), type="html_to_Rd-unsupported_tag"))
    } else
    doc_error(._("Cannot convert shiny.tag of type %1$s." %<<%
                 "%1$s is a not a valid HTML5 tag.", sQuote(html$name)))
}

html_to_Rd.code <- function(html, ...){
    assert_that( is.list(html$children)
               , all(purrr::map_lgl(html$children, is.character))
               )
    Rd_tag(as.character(html$children),'code')
}
if(FALSE){#@testing
    html <- htmltools::code("plot(rnorm(100))")
    rd <- html_to_Rd(html)
    expect_is(rd, 'Rd_tag')
    expect_is(rd, 'Rd')
    expect_identical(unclass(rd), "\\code{plot(rnorm(100))}")

    html <- htmltools::code("'a' %in% letters")
    rd <- html_to_Rd(html)
    expect_is(rd, 'Rd_tag')
    expect_is(rd, 'Rd')
    expect_identical(unclass(rd), "\\code{'a' \\%in\\% letters}")
}

html_to_Rd.strong <- function(html, ...){
    content <- Rd(lapply(html$children, html_to_Rd))
    Rd_tag(content, 'strong')
}
html_to_Rd.em <- function(html, ...){
    content <- lapply(html$children, html_to_Rd)
    Rd_tag(Rd(content), 'emph')
}

html_to_Rd.p <- function(html, ...){
    content <- lapply(html$children, html_to_Rd)
    if (length(content)==0) return(Rd(character(0)))
    content <- append(content,values = list(''))
    Rd(content)
}
if(FALSE){#@testing
    expect_is(html_to_Rd(htmltools::p()), 'Rd')
    expect_equal(unclass(html_to_Rd(htmltools::p())), character(0))
    expect_is(html_to_Rd(htmltools::p("text")), 'Rd')
    expect_equal(unclass(html_to_Rd(htmltools::p("text"))), c("text", ""))
}



if(FALSE){#Development
    e <- htmltools::em('with some emphatic text.')
    a <- htmltools::tags$p( "Some paragraph text", e)
    b <- htmltools::code("plot(rnorm(100))")
    x <- htmltools::tags$div( a, b)

    expect_is(html_to_Rd(e), 'Rd')
    expect_equal(unclass(html_to_Rd(e)), "\\emph{with some emphatic text.}")

    a <- htmltools::a("some text", href="https://r-project.org")

    html <- b
    html <- a

}

setOldClass('shiny.tag')
setMethod('toRd', 'shiny.tag', function(obj, ...)html_to_Rd(obj, ...))

