#' @include Fun-documentation.R


# Tag classifications ----------------------------------------------------------
.html5.tags.discouraged.reasons <-
    .T( b      = "Use strong"
      , i      = "Use em"
      , small  = "Do not use formatting tags."
      , sup    = "Do not use formatting tags."
      , sub    = "Do not use formatting tags."
      , u      = "Do not use formatting tags."
      )
.html5.tags.discouraged <- names(.html5.tags.discouraged.reasons)
.html5.tags.allowed <-
    .T( a, abbr, aside
      , blockquote, br
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
      , table, tbody, td, tfoot, th, thead, tr
      , ul
      )
.html5.tags.disallowed <-
    .T( area, address, article, audio
      , base, bdi, bdo, body
      , canvas, center, col, colgroup, command
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
      , textarea, time, title, track, tt
      , var, video, wbr
      )

# Utilities --------------------------------------------------------------------
# Condition Utilities ==========================================================
doc_warning_html5_discouraged <- function(tag, alt=NULL){
    doc_warning(._("Use of HTML direct formatting tags %s is discouraged.", dQuote(tag)) %<<%
                (if (!is.null(alt))
                    ._("We recommend using the tag %s as an alternate", dQuote(alt)) )
               , type = c("html_to_Rd", "html_to_Rd-discouraged"))
}
doc_error_html5_malformed <- function(tag, msg=NULL){
    doc_error(._("Malformed HTML in tag %s:", dQuote(tag)) %<<% msg
             , type = c("html_to_Rd", "html_to_Rd-malformed_html"))
}
html_conversion_information_loss <- function(tag, cond='warning'){
    doc_condition( type = c('html_to_Rd', 'html_to_Rd-info_loss')
                 , cond = cond
                 , ._('Extracting text from HTML tag %s, information will be lost'
                     , sQuote(tag))
                 )
}
html_get_type <- function(x){
    if (identical(class(x), 'list')) return(sapply(x, html_get_type))
    else if (inherits(x, 'shiny.tag')) return(x$name)
    else if (is.character(x)) return("")
    else doc_error(._("Cannot get HTML type from type %s", dQuote(class(x))))
}
if(FALSE){#@testing
    e <- htmltools::em('with some emphatic text.')
    a <- htmltools::tags$p( "Some paragraph text", e)
    b <- htmltools::code("plot(rnorm(100))")
    x <- htmltools::tags$div( a, b)

    expect_identical(html_get_type(x), 'div')
    expect_identical(html_get_type(x$children), c('p', 'code'))

    html <- htmltools::tags$li('with some emphatic text.')
    class(html) <- c('li', 'shiny.tag')
    expect_identical(html_get_type(html), 'li')

    expect_identical(html_get_type('character'), '')

    expect_error( html_get_type(NULL)
                , class = "documentation-error" )
}

html_is_type <- function(html, type){
    assert_that(inherits(html, 'shiny.tag'))
    html_get_type(html) == type
}
on_failure(html_is_type) <- function(call, env){
    actual.class <- html_get_type(eval(call$html, envir = env))
    expected.class <- eval(call$type, envir = env)
    deparse(call$html)  %<<%
        "is of type" %<<% dQuote(actual.class) %<<<%
        ";" %<<% "expected a" %<<% dQuote(expected.class)
}
if(FALSE){#@testing html_is_type
    a <- htmltools::a('link')
    expect_true(html_is_type(a, 'a'))

    withr::with_options(list(useFancyQuotes=FALSE),
                        expect_equal( see_if(html_is_type(a, 'li'))
                                      , s(FALSE, msg = 'a is of type "a"; expected a "li"')
                        ))
}

html_has_valid_children <-
function(html, allowed){
    ctypes <- html_get_type(html$children)
    good <- ctypes %in% allowed
    if (all(good)) return(TRUE)
    bad.tags <- sort(unique(ctypes[!good]))
    msg <- ._("HTML tag %s contains invalid child", html_get_type(html)) %<<%
           ngettext( sum(!good), "tag", "tags") %<<%
           ngettext( length(bad.tags), "of type", "of types") %<<%
           comma_list(bad.tags) %<<<% '.'
    s(FALSE, msg=msg)
}
if(FALSE){#@testing
    good.html <- with(htmltools::tags, ol(li('hello'), li("world") ))
    expect_true(see_if(html_has_valid_children(good.html, 'li')))

    bad.html <- with(htmltools::tags, ol(li('hello'), dt("term"), dd("definition") ))
    expect_equal( see_if(html_has_valid_children(bad.html, 'li'))
                , s(FALSE, msg="HTML tag ol contains invalid child tags of types dd and dt."))

    bad.html <- with(htmltools::tags, ol(li('hello'), dt("term"), dd("definition"), a("link") ))
    expect_equal( see_if(html_has_valid_children(bad.html, 'li'))
                , s(FALSE, msg="HTML tag ol contains invalid child tags of types a, dd, and dt."))

    bad.html <- with(htmltools::tags, ol(li('hello'), dt("term")))
    expect_equal( see_if(html_has_valid_children(bad.html, 'li'))
                , s(FALSE, msg="HTML tag ol contains invalid child tag of type dt."))

    bad.html <- with(htmltools::tags, ol(li('hello'), dt("term"), dt("term")))
    expect_equal( see_if(html_has_valid_children(bad.html, 'li'))
                , s(FALSE, msg="HTML tag ol contains invalid child tags of type dt."))
}

# Other Utilities ==============================================================
.protocols <- .T(http, https, ftp, mailto, file, data, irc)
url.pattern <- paste0('^(', collapse(.protocols, with='|'), ')://')
is_url <- function(x){grepl(url.pattern, x)}

pkg.base.pattern <- "[a-zA-Z][a-zA-Z0-9.]*[a-zA-Z0-9]"
pkg.pattern <-  "^" %<<<% pkg.base.pattern %<<<% "$"
name.pattern <- "[a-zA-Z.][a-zA-Z0-9_.]*(-[a-zA-Z]+)?"
pkg.dest.pattern <- "^" %<<<% pkg.base.pattern %<<<% ":" %<<<% name.pattern %<<<% "$"
is_rd_link <- function(x){
    (x == '') | grepl("^=", x) | grepl(pkg.pattern, x) | grepl(pkg.dest.pattern, x)
}
if(FALSE){#@testing
    expect_true(is_rd_link(''))
    expect_true(is_rd_link('=abc-class'))
    expect_true(is_rd_link('terms.object'))
    expect_true(is_rd_link('base:abc'))
    expect_false(is_rd_link("http://r-project.org"))
}

is_header <- function(html){
    inherits(html, 'shiny.tag') && grepl('h[1-6]', html$name)
}
if(FALSE){#@testing
    expect_true(is_header(htmltools::tags$h1("yes")))
    expect_false(is_header(htmltools::tags$b("yes")))
    expect_false(is_header("yes"))
}

# Generator Functions-----------------------------------------------------------

make_simple_html_converter <-
function(html.tag, rd.tag, allowed.children=NULL, envir = parent.frame()){
    assert_that(is.string(html.tag), is.string(rd.tag))
    args <- AL( html = arg_('html', 'a shiny.tag object of type' %<<<% html.tag)
              , '...' =arg_('...' , 'Ignored.')
              )
    fun <- if (!is.null(allowed.children)) {
        assert_that(is.character(allowed.children))
        expr <- substitute( env=list( allowed.children = allowed.children
                                    , html.tag=html.tag
                                    , rd.tag=rd.tag
                                    ),
            # nocov start
            function(html, ...){
                assert_that( html_is_type(html, html.tag)
                           , html_has_valid_children(html, allowed=allowed.children)
                           )
                Rd_tag(rd.tag, content = html_to_Rd(html$children))
            }
            # nocov end
        )
        s( eval(expr, envir=envir)
         , allowed.children = allowed.children
         , rd.tag = rd.tag
         )
    } else {
        # nocov start
        expr <- substitute(function(html, ...){
                assert_that( html_is_type(html, html.tag))
                Rd_tag(rd.tag, content=html_to_Rd(html$children))
            })
        # nocov end
        eval(expr, envir=parent.frame())
    }
    if (.document.generated) {
        docs <- function_documentation(
            title = "Convert HTML tag" %<<% sQuote(html.tag) %<<%
                    "to Rd tag" %<<% sQuote(rd.tag),
            description = FormattedText(
                "This function was generated by the make_simple_html_extractor" %<<%
                "function, and handles the simple conversion from shiny tags of type" %<<%
                sQuote(html.tag) %<<% "to the Rd direct equivalent" %<<% sQuote(rd.tag) %<<% '.'),
            arguments = args,
            value = FormattedText("A vector of mode character correctly escaped" %<<%
                                  "and classed as 'Rd_tag' and 'Rd'.")
            )
        documentation(fun) <- docs
    }
    attr(fun, 'srcref') <- NULL
    fun
}
if(FALSE){#@testing
    test_fun <- make_simple_html_converter('htmltag', 'rdtag')
    expect_identical( formals(test_fun)
                    , as.pairlist(alist(html=, ...=))
                    )

    expect_identical( trimws(deparse(body(test_fun), 500))
                    , c( '{'
                       , "assert_that(html_is_type(html, \"htmltag\"))"
                       , "Rd_tag(\"rdtag\", content = html_to_Rd(html$children))"
                       , "}"
                       )
                    )
    expect_equal( isTRUE(is_documented('test_fun', environment(), complete=FALSE))
                , .document.generated)

    html <- htmltools::tag('htmltag', varArgs = list('content'))
    val <- test_fun(html)
    expect_is(val, 'Rd_tag')
    expect_identical(val, Rd_tag("rdtag", Rd_text("content")))


    expect_error(test_fun(htmltools::tag('not the right tag', varArgs = list('content'))))

    test_children <- make_simple_html_converter('htmltag', 'rdtag', allowed.children = c('tag1', 'tag2'))
    expect_is(test_children, 'function')

    expect_identical( trimws(deparse(body(test_children), 500))
                    , c( '{'
                       , 'assert_that(html_is_type(html, "htmltag"),' %<<%
                            'html_has_valid_children(html, allowed = c("tag1", "tag2")))'
                       , 'Rd_tag("rdtag", content = html_to_Rd(html$children))'
                       , '}'
                       )
                    )
}


html_simple_extractor <-
function( html
        , warn.info.loss = default(warn.info.loss, 'warning', fun='Rd')
        , ...
        ){
    assert_that(inherits(html, "shiny.tag"))
    html_conversion_information_loss(html$name, warn.info.loss)
    html_to_Rd(html$children)
}
if(FALSE){#@testing
    html <- htmltools::tag('htmltag', varArgs = list('content'))
    expect_warning( val <- html_simple_extractor(html, warn.info.loss='warn')
                  , class =  "documentation-warning-html_to_Rd-info_loss")
    expect_is(val, 'Rd')
    expect_identical(val, Rd(Rd_text("content")))
}

# html_to_Rd -------------------------------------------------------------------
#' Convert html shiny.tag objects to Rd.
#'
#' Dispatchs on on the type html tag, ie. the 'name' attribute.
#' @export
html_to_Rd <- function (html, ...) {
    if (is_exactly(html, "shiny.tag"))
        html <- cl(html, html_get_type(html))
    UseMethod("html_to_Rd", html)
}

#' @export
html_to_Rd.default <- function(html, ...){
    doc_error(._("Cannot convert %1$s to HTML.", class(html)))
}
if(FALSE){#@testing
    expect_error( html_to_Rd(1L), class='documentation-error')
}

#' @S3method html_to_Rd character
html_to_Rd.character <- function(html, ...){
    assert_that(is.character(html))
    return(Rd_text(clean_Rd(html)))
}
if(F){#@testing
    val <- html_to_Rd.character('content')
    expect_is_exactly(val, 'Rd_TEXT')
    expect_identical(val, Rd_text("content"))
}

#' @S3method html_to_Rd list
html_to_Rd.list <- function(html, ...){
    assert_that(is_exactly(html, 'list'))
    if (length(html)==0) return(Rd())
    content <- lapply(html, html_to_Rd, ...)
    return(toRd(content))
}
if(F){#@testing
    html <- htmltools::tag('htmltag', varArgs = list('content'))$children
    lapply(html, html_to_Rd)
    val <- html_to_Rd.list(html)
    expect_is_exactly(val, 'Rd')
    expect_identical(val, Rd(Rd_text('content')))
}

#' @S3method html_to_Rd Rd
html_to_Rd.Rd <- function(html, ...){
    assert_that( is(html, 'Rd') )
    html
}
if(FALSE){#@testing
    expect_identical( html_to_Rd(Rd("hi")), Rd("hi"))
    expect_error(html_to_Rd.Rd("text") )
}

#' @S3method html_to_Rd shiny.tag
html_to_Rd.shiny.tag <- function(html, ...){
    assert_that(inherits(html, 'shiny.tag'))
    tryCatch({
        type <- html_get_type(html)
        class(html) <- c(type, 'shiny.tag')
        utils::getS3method('html_to_Rd', type)(html, ...)
    }, error = function(e){
        if (inherits(e, 'documentation-error')) stop(e)
        doc_error(._("Cannot convert shiny.tag of type %1$s." %<<%
                     "While %1$s is a valid HTML5 tag, documentation" %<<%
                     "does not currently support it's conversion to Rd."
                    , sQuote(html$name), type="html_to_Rd-unsupported_tag"))
    })
}

# Generated Methods ============================================================

### <aside> #####
### @S3method html_to_Rd aside
html_to_Rd.aside <- make_simple_html_converter('aside', 'note')
if(FALSE){
    html <- htmltools::tags$aside(c("Just a note", "that in html is called an aside."))
    val <- html_to_Rd(html)

    expect_is(val, 'Rd')
    expect_identical( val
                    , Rd_tag( "note"
                            , Rd_text("Just a note" %<<<%
                                      "that in html is called an aside."
                                     )))
}

### <em> #####
### @S3method html_to_Rd em
html_to_Rd.em <- make_simple_html_converter('em','emph')

### <cite> #####
#' @S3method html_to_Rd cite
html_to_Rd.cite <- make_simple_html_converter('cite', 'cite')
if(FALSE){
    html <- htmltools::tags$cite("a citation")
    val <- html_to_Rd(html)
    expect_is(val, 'Rd_tag')
    expect_identical(val, Rd_tag('cite', Rd_text("a citation")))
    expect_equal(collapse0(val), "\\cite{a citation}")
}

### <h1>...<h6> #####
#' @S3method html_to_Rd h1
#' @S3method html_to_Rd h2
#' @S3method html_to_Rd h3
#' @S3method html_to_Rd h4
#' @S3method html_to_Rd h5
#' @S3method html_to_Rd h6
html_to_Rd.h1 <-
html_to_Rd.h2 <-
html_to_Rd.h3 <-
html_to_Rd.h4 <-
html_to_Rd.h5 <-
html_to_Rd.h6 <-
    html_simple_extractor

### <kbd> #####
#' @S3method html_to_Rd kbd
html_to_Rd.kbd <- make_simple_html_converter('kbd', 'kbd')
if(FALSE){#@testing
    html <- htmltools::tags$kbd("abc")
    val <- html_to_Rd(html)
    expect_is(val, 'Rd')
    expect_identical( val, Rd_tag("kbd", Rd_text("abc")))
}


### <pre> #####
#' @S3method html_to_Rd pre
html_to_Rd.pre <- make_simple_html_converter("pre", "preformatted")

### <q> #####
#' @S3method html_to_Rd q
html_to_Rd.q <- make_simple_html_converter("pre", "dQuote")

### <samp> #####
#' @S3method html_to_Rd samp
html_to_Rd.samp <- make_simple_html_converter("samp", "preformatted")

### <strong> #####
#' @S3method html_to_Rd strong
html_to_Rd.strong <- make_simple_html_converter('strong','strong')


# Extractor Methods ============================================================


### <span> #####
#' @S3method html_to_Rd span
html_to_Rd.span <- html_simple_extractor

### <td> #####
#' @S3method html_to_Rd td
html_to_Rd.td <- html_simple_extractor


# Discouraged Methods ==========================================================

#' @S3method html_to_Rd b
html_to_Rd.b <- function(html, ...){
    doc_warning_html5_discouraged('b', 'strong')
    content <- html_to_Rd(html$children, ...)
    Rd_tag('b', content = content)
}
if(FALSE){#@testing
    html <- htmltools::tags$b("something to bold")
    expect_warning( val <- html_to_Rd(html)
                  , class = "documentation-warning-html_to_Rd-discouraged")
    expect_is(val, 'Rd_tag')
    expect_identical(collapse0(val), "\\b{something to bold}")
}


# Complex Methods ==============================================================

#' @S3method html_to_Rd a
html_to_Rd.a <- function(html, ...){
    href <- html$attribs$href
    assert_that(all(purrr::map_lgl(html$children, is.character)))
    if (is.null(href)) return(Rd_tag('link', content=html_to_Rd(html$children)))
    if (is_rd_link(href))
        return(Rd_tag('link', content=html_to_Rd(html$children), opt=Rd_text(href)))
    if (is_url(href))
        return(Rd_tag('href'
                     , Rd(Rd_text(href, type = "VERB"))
                     , Rd(html_to_Rd(html$children))))
    doc_error(._("Don't know how to interpret href=%s", dQuote(href))
             , type = 'html_to_Rd' )
}
if(FALSE){#@testing
    expect_identical( html_to_Rd(htmltools::a("somewhere"))
                    , Rd_tag("link", Rd_text("somewhere"))
                    )
    html <- a <- htmltools::a("some text", href="https://r-project.org")

    val <- html_to_Rd(htmltools::a("some text", href="https://r-project.org"))
    expect_is(val, 'Rd_tag')
    expect_identical( val
                    , Rd_tag("href"
                            , Rd(Rd_text("https://r-project.org", 'VERB'))
                            , Rd("some text")
                            ) -> expected)

    val <- html_to_Rd(html <- htmltools::a("some text", href="abc"))
    expect_is(val, 'Rd_tag')
    expect_identical(collapse0(val), "\\link[abc]{some text}")

    val <- html_to_Rd(htmltools::a("some text", href="=abc-class"))
    expect_is(val, 'Rd_tag')
    expect_identical(collapse0(val), "\\link[=abc-class]{some text}")

    val <- html_to_Rd(htmltools::a("some text", href="pkg:dest"))
    expect_is(val, 'Rd_tag')
    expect_identical(collapse0(val), "\\link[pkg:dest]{some text}")

    val <- html_to_Rd(htmltools::a("some text", href="=somewhere"))
    expect_is(val, 'Rd_tag')
    expect_identical(collapse0(val), "\\link[=somewhere]{some text}")

    expect_error( html_to_Rd.a(htmltools::a("some text", href="somewhere over the rainbow"))
                , class = "documentation-error-html_to_Rd" )
    expect_error( html_to_Rd(htmltools::a("some text", href="somewhere over the rainbow"))
                , class = "documentation-error-html_to_Rd" )
}

#' @S3method html_to_Rd abbr
html_to_Rd.abbr <- function(html, ...){
    assert_that( all(purrr::map_lgl(html$children, is.character))
               , length(html$children) == 1L
               )
    content <- html_to_Rd(html$children[[1]])
    if (!grepl("^[A-Z]+$", content))
        doc_warning(._( "HTML tag %s maps to Rd tag acronym, " %<<%
                           "It should consist of all capitol letters."
                      , dQuote(html$name))
                    , type = "html_to_Rd" )
    Rd_tag('acronym', content=Rd(content))
}
if(FALSE){#@testing
    html <- htmltools::tags$abbr("GPL")
    val <- html_to_Rd(html)
    expect_is(val, 'Rd_tag')
    expect_identical(collapse0(val), "\\acronym{GPL}")
    expect_identical(val, Rd_tag("acronym", Rd_text("GPL")))

    expect_warning( html_to_Rd(htmltools::tags$abbr("not an acronym"))
                  , class = 'documentation-warning-html_to_Rd')
    expect_error( html_to_Rd(with(htmltools::tags, abbr( "not an acronym"
                                                       , b("not valid")))))

}

#' @S3method html_to_Rd br
html_to_Rd.br <- function(html, ...){
    assert_that( html_is_type(html, 'br'))
    if (length(html$children) > 0)
        doc_error_html5_malformed('br', "cannot have children.")
    return(Rd_tag('cr'))
}
if(FALSE){#@testing
    html <- htmltools::tags$br()
    val <- html_to_Rd(html)
    expect_is(val, 'Rd')
    expect_identical(collapse0(val), "\\cr")

    html <- with(htmltools::tags, br('text'))
    expect_error( html_to_Rd.br(html)
                , class = "documentation-error-html_to_Rd-malformed_html")
    expect_error( html_to_Rd(html)
                , class = "documentation-error-html_to_Rd-malformed_html")
}

#' @S3method html_to_Rd code
html_to_Rd.code <- function(html, ...){
    assert_that( is.list(html$children)
               , all(purrr::map_lgl(html$children, is.character))
               )
    Rd_canonize(Rd_tag("code", Rd_rcode(clean_Rd(as.character(html$children)))))
}
if(FALSE){#@testing
    html <- htmltools::code("plot(rnorm(100))")
    rd <- html_to_Rd(html)
    expect_is(rd, 'Rd_tag')
    expect_is(rd, 'Rd')
    expect_true(is_Rd_tag(rd, '\\code'))
    expect_identical(collapse0(rd), "\\code{plot(rnorm(100))}")

    html <- htmltools::code("'a' %in% letters")
    rd <- html_to_Rd(html)
    expect_is(rd, 'Rd_tag')
    expect_is(rd, 'Rd')
    expect_identical(collapse0(rd), "\\code{'a' \\%in\\% letters}")
}

#' @S3method html_to_Rd div
html_to_Rd.div <- function(html, sub.section=FALSE, ...){
    assert_that( is.flag(sub.section))
    children <- html$children
    if (!is.null(html$attribs$title))
        title <- Rd(html$attribs$title) else
    if (is_header(children[[1]])) {
        title <- html_to_Rd(children[[1]], warn.info.loss='none')
        children <- children[-1]
    } else
        doc_error_html5_malformed('div', "cannot determine title of section.")
    content <- html_to_Rd(children, sub.section=TRUE)
    if ( Rd_spans_multiple_lines(content) ){
        if ( !Rd_starts_with_newline(content)) content <- c(.Rd.newline, content)
        if ( !Rd_ends_with_newline(content)) content <- c(content, .Rd.newline)
    }

    s( list(title=title, content=content)
     , Rd_tag=ifelse(sub.section, "\\subsection", "\\section")
     , class =c('Rd_tag', 'Rd')
     )
}
if(FALSE){#@testing
    txt <- lapply(stringi::stri_rand_lipsum(3), htmltools::tags$p)
    title <- "test title"
    html <- htmltools::tags$div(title=title)
    html$children <- txt
    val <- html_to_Rd(html)
    expect_is(val, 'Rd')
    expect_true(is_Rd_tag(val, "\\section"))

    h <- htmltools::tags$h3("Embedded Title")
    html <- htmltools::tags$div(h, txt)
    val <- html_to_Rd(html)
    expect_is(val, 'Rd')
    expect_true(is_Rd_tag(val, "\\section"))
    expect_length(val, 2L)
    expect_identical(val[[1]], Rd('Embedded Title'))

    ss <-   htmltools::tags$div( htmltools::tags$h4("Sub-section Header")
                               , stringi::stri_rand_lipsum(2))
    sec <- htmltools::tags$div( htmltools::tags$h3("Section Header")
                              , ss)
    val <- html_to_Rd(sec)
    expect_is(val, 'Rd')
    expect_true(is_Rd_tag(val, '\\section'))
    expect_is_exactly(val[[2]], 'Rd')
    expect_true(is_Rd_tag(val[[2]][[1]], '\\subsection'))

    expect_error( html_to_Rd(with(htmltools::tags, div( em("Section Header"), ss)))
                , class="documentation-error-html_to_Rd-malformed_html" )
}

#' @S3method html_to_Rd dt
html_to_Rd.dfn <- function(html, ...){
    assert_that( length(html$children) == 1
               , is.character(html$children[[1]])
               )
    Rd_tag('dfn', Rd_text(html$children[[1]]))
}
if(FALSE){#@testing
    rd <- html_to_Rd(html <- htmltools::tags$dfn("abc"))
    expect_identical(rd, Rd_tag('dfn', Rd_text("abc")))
}

#' @S3method html_to_Rd dt
html_to_Rd.dt <- function(html, ...){
    Rd(html_to_Rd(html$children))
}

#' @S3method html_to_Rd dd
html_to_Rd.dd <- function(html, ...){
    Rd(html_to_Rd(html$children))
}

#' @S3method html_to_Rd dl
html_to_Rd.dl <- function(html, ...){
    assert_that( length(html$children) %% 2 == 0
               , all( sapply(html$children, `[[`, 'name') %in% c('dd', 'dt') )
               )
    items <- vector("list", length(html$children) %/% 2)
    for( i in 1:length(items)){
        j <- (i-1)*2L + 1L
        dt <- html$children[[j]]
        dd <- html$children[[j+1]]
        if ( dt$name != 'dt' || dd$name != 'dd' )
            doc_error_html5_malformed('dl',
              ._("HTML tags %s and %s occure in the wrong order."
                , sQuote('dt'), sQuote('dd')))
        items[[i]] <- Rd_item( arg = html_to_Rd(dt)
                             , description = html_to_Rd(dd, warn.info.loss='none')
                             )
    }
    if (length(items) > 1)
        items <- c(.Rd.newline, cl(undim(rbind(items, .Rd.newline)), 'Rd'))
    else
        items <- cl(items, 'Rd')
    Rd_tag('describe', content=.Rd_indent(items, ...))
}
if(FALSE){#@testing
    html <- htmltools::tags$dl( htmltools::tags$dt("term1")
                                , htmltools::tags$dd("definition 1.")
                              , htmltools::tags$dt("term2")
                                , htmltools::tags$dd("definition 2.")
                              )
    val <- html_to_Rd(htmltools::tags$dt("my term"))
    expect_is(val, 'Rd')
    expect_equal(collapse0(val), "my term" )

    val <- html_to_Rd(htmltools::tags$dd("definition 1."))
    expect_identical(val, Rd("definition 1."))

    val <- html_to_Rd(html)
    expect_is(val, 'Rd')
    expect_equal( collapse0(val)
                , "\\describe{" %\%
                  "\\item{term1}{definition 1.}" %\%
                  "\\item{term2}{definition 2.}" %\%
                  "}"
                )

    val <- html_to_Rd(html, indent=TRUE, indent.with='  ')
    expect_is(val, 'Rd')
    expect_equal( collapse0(val)
                , "\\describe{" %\%
                  "  \\item{term1}{definition 1.}" %\%
                  "  \\item{term2}{definition 2.}" %\%
                  "}"
                )


    html <- htmltools::tags$dl( htmltools::tags$dt("term1")
                                , htmltools::tags$dd("definition 1.")
                                , htmltools::tags$dd("definition 2.")
                              , htmltools::tags$dt("term2")
                              )
    expect_error( html_to_Rd(html)
                , class="documentation-error-html_to_Rd-malformed_html" )


}

#' @S3method html_to_Rd html
html_to_Rd.html <- function(html, ...){
    html_to_Rd(html$children)
}
if(FALSE){#@testing
    html1 <- htmltools::tags$dl( htmltools::tags$dt("term1")
                                 , htmltools::tags$dd("definition 1.")
                               , htmltools::tags$dt("term2")
                                 , htmltools::tags$dd("definition 2.")
                               )

    html2 <- htmltools::tags$html(html1)

    expect_identical( html_to_Rd(html2)[[1]]
                    , html_to_Rd(html1)
                    )
}


#' @S3method html_to_Rd img
html_to_Rd.img <- function(html, ...){
    assert_that( length(html$children) == 0)
    src <- html$attribs$src
    if (is.null(src)) doc_error_html5_malformed('img', ._("No src defined."))
    opts <- html$attribs[names(html$attribs) != 'src']
    name <- "figure{" %<<<% Rd(src) %<<<% '}'

    opts <- 'options:' %<<% paste0(names(opts), '=', sapply(opts, deparse))
    Rd_tag( 'figure'
          , Rd(src)
          , Rd(opts)
          )
}
if(FALSE){#@testing
    html <- htmltools::tags$img(src='test.png', alt ='alternate text', height=100, width=100)
    val <- html_to_Rd(html)
    expect_is(val, 'Rd')
    expect_true(is_Rd_tag(val, '\\figure'))
    expect_equal( collapse0(val)
                , "\\figure{test.png}{options: alt=\"alternate text\" height=100 width=100}")

    html <- htmltools::tags$img(alt ='alternate text', height=100, width=100)
    expect_error(html_to_Rd(html), class="documentation-error")
}

#' @S3method html_to_Rd li
html_to_Rd.li <- function(html, ...){
    Rd_canonize(c( Rd_tag("item"), Rd_text(space(1))
                 , html_to_Rd(html$children)
                 ))
}
if(FALSE){#@testing
    html <- htmltools::tags$li("some ", htmltools::tags$em('text'), '.')
    val <- html_to_Rd(html)
    expect_is(val, 'Rd')
    expect_identical( val, Rd( Rd_tag('item'), Rd_text(" some ")
                             , Rd_tag('emph', Rd_text("text"))
                             , Rd_text(".")))
}


#' @S3method html_to_Rd p
html_to_Rd.p <- function(html, ...){
    content <- html_to_Rd(html$children)
    if (length(content)==0) return(Rd())
    c(content, .Rd.break)
}
if(FALSE){#@testing
    html <- htmltools::tags$p(stringi::stri_rand_lipsum(1))

    val <- html_to_Rd(html)
    expect_is_exactly(val, 'Rd')
    expect_is(val[[2]], 'Rd_break')

    expect_is(html_to_Rd(htmltools::p()), 'Rd')
    expect_identical(html_to_Rd(htmltools::p()), Rd())
    expect_is(html_to_Rd(htmltools::p("text")), 'Rd')
    expect_equal(collapse0(html_to_Rd(htmltools::p("text"))), c("text\n\n"))
}

#' @S3method html_to_Rd ol
html_to_Rd.ol <-
function( html, ...){
    assert_that( html_is_type(html, "ol")
               , html_has_valid_children(html, allowed = "li"))
    items <- lapply(html$children, html_to_Rd)
    if (length(items) > 1)
        items <- compact_Rd(c(undim(rbind(.Rd.newline, items)), .Rd.newline))
    Rd_tag('enumerate', content=Rd_canonize(items, ...))
}
if(FALSE){#@testing
    html <- htmltools::tags$ol( htmltools::tags$li("First")
                              , htmltools::tags$li("Second")
                              )
    val <- html_to_Rd(html)
    expect_is(val, 'Rd_tag')
    expected <- Rd_tag( "enumerate", .Rd.newline[[1L]]
                      , Rd_tag("item"), Rd_text(" First\n")
                      , Rd_tag("item"), Rd_text(" Second\n")
                      )
    expect_identical( val, expected)

    val <- html_to_Rd(html, indent=TRUE, indent.with='  ')
    expect_identical( as.character(val)
                    , c( "\\enumerate", '{', '\n'
                       , "  ", "\\item", " First\n"
                       , "  ", "\\item", " Second\n"
                       , "}"))
    expect_error( html_to_Rd(htmltools::tags$ol( htmltools::tags$li("First")
                                               , htmltools::tags$dl("Second")
                                               )))
}


#' @S3method html_to_Rd section
html_to_Rd.section <- html_to_Rd.div


#' @S3method html_to_Rd table
html_to_Rd.table <-
function( html
        , ...
        , col.align = default(col.align, 'l') #< default column alignment.
        ){
    allowed.chilren <- c('thead', 'tbody', 'tfoot')
    ctypes <- html_get_type(html$children)
    assert_that( all(ctypes %in% allowed.chilren)
               , !anyDuplicated(ctypes)
               )
    if (!any(ctypes == 'tbody'))
        doc_error_html5_malformed(._("HTML table element must contain a tbody."))

    body <- html_to_Rd(html$children[[which(ctypes == 'tbody')]], ...)
    head <- if(any(. <- ctypes == 'thead'))
        html_to_Rd(html$children[[which(.)]], ...)
    foot <- if(any(. <- ctypes == 'tfoot'))
        html_to_Rd(html$children[[which(.)]], ...)

    if (!is.null(head))
        assert_that( attr(body, 'ncols') == attr(head, 'ncols')
                   , msg = "HTML tbody and thead do not have the same number of columns")
    if (!is.null(foot))
        assert_that( attr(body, 'ncols') == attr(foot, 'ncols')
                   , msg = "HTML tbody and thead do not have the same number of columns")

    nrows <- attr(body, 'nrows') +
            (attr(head, 'nrows') %||% 0) +
            (attr(foot, 'nrows') %||% 0)
    ncols <- attr(body, 'ncols')
    align <- Rd(str_rep(col.align, ncols))

    content <- c( .Rd.newline
                , cl(rbind( list(c(Rd_tag('cr'), .Rd.newline))
                          , list(head, body, foot)
                          )[-1], 'Rd')
                , .Rd.newline)
    content<- Rd_canonize(compact_Rd(content), ...)
    s( list(align, content)
     , Rd_tag="\\tabular"
     , class=c('Rd_tag', 'Rd')
     )
}

#' @S3method html_to_Rd th
html_to_Rd.th <-
function( html
        , warn.info.loss = default(warn.info.loss, 'warning', fun='Rd')
        , ...) {
    html_simple_extractor( html=html
                         , warn.info.loss=warn.info.loss
                         , ...)
}

#' @S3method html_to_Rd tr
html_to_Rd.tr <- function(html, head=FALSE, ...){
    allowed.children <- if (head) 'th' else 'td'
    assert_that( all(html_get_type(html$children) %in% allowed.children) )
    cols <- lapply(html$children, html_to_Rd, ...)

    nbsp <- Rd_text(space(1))
    rd <- compact_Rd(undim(rbind( list(Rd(nbsp, Rd_tag('tab'), nbsp))
                                , cols
                                ))[-1L])
    s(Rd_canonize(rd), ncols = length(cols))
}

#' @S3method html_to_Rd tbody
html_to_Rd.tbody <-
function( html, ...
        , warn.info.loss  = default(warn.info.loss, 'none')
        ){
    assert_that( all(html_get_type(html$children) %in% 'tr') )
    rows <- lapply( html$children, html_to_Rd, ...
                  , warn.info.loss=warn.info.loss )
    if (length(rows)==0) return(Rd())

    ncols <- max(purrr::map_int(rows, attr, 'ncols'))

    if (length(rows) > 1L)
        rows <- undim( rbind(list(c(Rd_tag("cr"), .Rd.newline))
                            , rows))[-1L]
    rows <- Rd_canonize(compact_Rd(rows))
    s( rows, ncols = ncols, nrows = length(html$children), class='Rd')
}

#' @S3method html_to_Rd tfoot
html_to_Rd.tfoot <-
function(html, ..., warn.info.loss  = default(warn.info.loss, 'message')){
    if(length(html$children)==0) return(Rd())
    html_conversion_information_loss('tfoot', cond = warn.info.loss)
    html_to_Rd.tbody(html, warn.info.loss = warn.info.loss, ...)
}

#' @S3method html_to_Rd thead
html_to_Rd.thead <-
function( html, ...
        , warn.info.loss  = default(warn.info.loss, 'warn')
        ){
    html_to_Rd.tbody( html, ...
                    , warn.info.loss=warn.info.loss
                    , head=TRUE
                    )
}
if(FALSE){#@testing html_to_Rd.* table functions
    html <-
        with(htmltools::tags, {
            table( thead( tr( th(''), th('C1'), th('C2'), th('C3') ) )
                 , tbody( tr( td('R1'), td('O'), td('X'), td('O') )
                        , tr( td('R2'), td('X'), td('X'), td('O') )
                        , tr( td('R3'), td('O'), td('X'), td('X') )
                        )
                 , tfoot( tr( td('Count'), td('1'), td('3'), td('1') ) )
                 , align = "right|center|center|center"
                 )
        })
    thead <- html$children[[1]]
    tbody <- html$children[[2]]
    tfoot <- html$children[[3]]

    expect_warning( head <- html_to_Rd(thead)
                  , class =  "documentation-warning-html_to_Rd-info_loss")
    expect_is_exactly(head, 'Rd')
    expect_length(head, 7L)
    expect_equal(attr(head, 'ncols'), 4L)
    expect_equal(attr(head, 'nrows'), 1L)
    expect_equal(collapse0(head), " \\tab C1 \\tab C2 \\tab C3")

    body <- html_to_Rd(tbody)
    expect_equal( collapse0(body)
                , "R1 \\tab O \\tab X \\tab O\\cr" %\%
                  "R2 \\tab X \\tab X \\tab O\\cr" %\%
                  "R3 \\tab O \\tab X \\tab X"
                )

    expect_message( foot <- html_to_Rd(tfoot)
                  , class =  "documentation-message-html_to_Rd-info_loss")

    expect_equal( collapse0(foot)
                , "Count \\tab 1 \\tab 3 \\tab 1"
                )

    val <- html_to_Rd(html, warn.info.loss='none')
    expect_is(val, 'Rd')
    expect_true(is_Rd_tag(val, '\\tabular'))
    expect_length(val, 2L)
    expect_is_exactly(val[[1]], 'Rd')
    expect_is_exactly(val[[2]], 'Rd')
    expect_equal( collapse0(val)
                , "\\tabular{llll}{" %\%
                  " \\tab C1 \\tab C2 \\tab C3\\cr" %\%
                  "R1 \\tab O \\tab X \\tab O\\cr" %\%
                  "R2 \\tab X \\tab X \\tab O\\cr" %\%
                  "R3 \\tab O \\tab X \\tab X\\cr" %\%
                  "Count \\tab 1 \\tab 3 \\tab 1" %\%
                  "}"
                )

    html <-
        with(htmltools::tags, {
            table( thead( tr( th(''), th('C1'), th('C2'), th('C3') ) )
                 , tfoot( tr( td('Count'), td('1'), td('3'), td('1') ) )
                 , align = "right|center|center|center"
                 )
        })
    expect_error(html_to_Rd(html), class="documentation-error-html_to_Rd-malformed_html")

    html <-
        with(htmltools::tags, {
            table( thead( tr( th(''), th('C1'), th('C2'), th('C3') ) )
                 , tfoot( tr( td('Count'), td('1'), td('3'), td('1') ) )
                 , align = "right|center|center|center"
                 )
        })
    expect_error(html_to_Rd(html), class="documentation-error-html_to_Rd-malformed_html")


    expect_identical( html_to_Rd(htmltools::tags$thead()), Rd())
    expect_identical( html_to_Rd(htmltools::tags$tbody()), Rd())
    expect_identical( html_to_Rd(htmltools::tags$tfoot()), Rd())

    thead <- with(htmltools::tags,
                  thead( tr( th(''), th('C'), th('D'), th('E') )
                       , tr( th('.'), th('1'), th('2'), th('3') )
                       ))
    val <- html_to_Rd( thead, collapse.lines=TRUE
                     , warn.info.loss = 'none'
                     )
    expect_is(val, 'Rd')
    expect_identical( collapse0(val)
                    , " \\tab C \\tab D \\tab E\\cr" %\%
                      ". \\tab 1 \\tab 2 \\tab 3"
                    )
    val <- html_to_Rd( thead, collapse.lines=FALSE
                     , warn.info.loss = 'none'
                     )
    expect_is(val, 'Rd')
    expect_identical( collapse0(val)
                    , " \\tab C \\tab D \\tab E\\cr" %\%
                      ". \\tab 1 \\tab 2 \\tab 3"
                    )

    tfoot <- with(htmltools::tags,
                tfoot( tr( td('Count'), td('1'), td('2'), td('3') )
                     , tr( td('Total'), td('A'), td('B'), td('C') )
                     ))

    val <- html_to_Rd( tfoot, collapse.lines=TRUE
                     , warn.info.loss = 'none'
                     )
    expect_is(val, 'Rd')
    expect_identical( collapse0(val)
                    , "Count \\tab 1 \\tab 2 \\tab 3\\cr" %\%
                      "Total \\tab A \\tab B \\tab C"
                    )

    val <- html_to_Rd( tfoot, collapse.lines=FALSE
                     , warn.info.loss = 'none'
                     )
    expect_is(val, 'Rd')
    expect_identical( collapse0(val)
                    , "Count \\tab 1 \\tab 2 \\tab 3\\cr" %\%
                      "Total \\tab A \\tab B \\tab C"
                    )
}

#' @S3method html_to_Rd ul
html_to_Rd.ul <-
function( html, ...){
    assert_that( html_is_type(html, "ul")
               , html_has_valid_children(html, allowed = "li"))
    items <- lapply(html$children, html_to_Rd)
    if (length(items) > 1)
        items <- compact_Rd(c(undim(rbind(.Rd.newline, items)), .Rd.newline))
    Rd_tag('itemize', content=Rd_canonize(items, ...))
}
if(FALSE){#@testing
    html <- htmltools::tags$ul( htmltools::tags$li("First")
                              , htmltools::tags$li("Second")
                              )
    val <- html_to_Rd(html)
    expect_is(val, 'Rd_tag')
    expect_identical(val, Rd_tag( "itemize", .Rd.newline[[1L]]
                                , Rd_tag("item"), Rd_text(" First\n")
                                , Rd_tag("item"), Rd_text(" Second\n")
                                ))
    expect_equal( collapse0(val)
                , "\\itemize{" %\%
                  "\\item First" %\%
                  "\\item Second" %\%
                  "}"
                )

    indent.with <- Rd_clean_indent('  ')
    val <- html_to_Rd(html, indent=TRUE, indent.with=indent.with)
    expect_is(val, 'Rd_tag')
    expect_identical(val, Rd_tag( "itemize", .Rd.newline[[1L]]
                                , indent.with[[1L]], Rd_tag("item"), Rd_text(" First\n")
                                , indent.with[[1L]], Rd_tag("item"), Rd_text(" Second\n")
                                ))
    expect_equal( collapse0(val)
                , "\\itemize{" %\%
                  "  \\item First" %\%
                  "  \\item Second" %\%
                  "}"
                )

    expect_error( html_to_Rd(htmltools::tags$ol( htmltools::tags$li("First")
                                               , htmltools::tags$dl("Second")
                                               ))
                )
}


# toRd,shiny.tag ------------------------------------------------------------------

setMethod('toRd', 'shiny.tag', function(obj, ...){
    if (identical(class(obj), 'shiny.tag'))
        class(obj) <- c(html_get_type(obj), 'shiny.tag')
    html_to_Rd(obj, ...)
})
if(FALSE){#@testing
    html <-
        with(htmltools::tags, {
            table( thead( tr( th(''), th('C1'), th('C2'), th('C3') ) )
                 , tbody( tr( td('R1'), td('O'), td('X'), td('O') )
                        , tr( td('R2'), td('X'), td('X'), td('O') )
                        , tr( td('R3'), td('O'), td('X'), td('X') )
                        )
                 , tfoot( tr( td('Count'), td('1'), td('3'), td('1') ) )
                 , align = "right|center|center|center"
                 )
        })
    rd <- toRd(html, warn.info.loss='none')
    expect_is_exactly(rd, 'Rd')
    expect_is_exactly(rd[[1]], 'Rd_tag')

    expect_identical( rd[[1]]
                    , html_to_Rd(html, warn.info.loss='none')
                    )
}
