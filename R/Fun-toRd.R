#' @include Classes.R
#' @include Fun-default.R
#' @importFrom tools toRd



### toRd #######################################################################
#' @export
setGeneric('toRd',
function(obj, ...){
    ans <- standardGeneric("toRd")
    if (is_exactly(ans, 'character')) ans <- Rd_canonize_text(Rd_text(ans))
    if (is.character(ans)) ans <- Rd_canonize(Rd(ans))
    if (is_exactly(ans, 'Rd')) return(ans)
    if (is(ans, 'Rd')) return(s(list(ans), class='Rd'))
    else
        doc_error(._( "Method of generic function %1$s for class %2$s" %<<%
                      "returned a %4$s." %<<%
                      "Methods of %1$s are expected to retun a %3$s vector."
                    , sQuote("toRd"), dQuote(class(obj))
                    , 'character', dQuote(class(ans))
                    ))
})
toRd@valueClass <- 'Rd'
if(FALSE){#@testing
    val <- toRd('character')
    expect_identical(val, Rd("character"))

    val <- toRd(c( "use the \\backslash to escape."
                 , "and '{}' to group."
                 ))
    expect_is(val, 'Rd')
    expect_identical( val
                    , Rd(Rd_text("use the \\\\backslash to escape." %<<<%
                                 "and '\\{\\}' to group."
                                 )))
    obj <-
        c( person('Andrew', 'Redd', email='andrew.redd@hsc.utah.edu')
                                  , person('Drew'  , 'Blue')
                                  )
    expect_identical( toRd(obj)
                    , Rd(Rd_text("Andrew Redd \\email{andrew.redd@hsc.utah.edu} and Drew Blue"))
                    )
}
if(FALSE){#@testing
    toRd.test_class <- function(obj, ...)obj
    expect_error( toRd(cl(1L, 'test_class'))
                , class = "documentation-error")
}
set_option_documentation( "documentation::Rd::indent"
   , description = "Determines if code should be indented when formatted.  Should default to FALSE when unset."
   , default = FALSE
   , constraints = list(is.logical)
   )
set_option_documentation( "documentation::Rd::indent.with"
   , description = "Determines what to indent with, when getOption('documentation::toRd::indent') is TRUE."
   , default = "  "
   , constraints = list(~is.string(.))
   )

# Helpers ============================================
.Rd.default.indent <- s(list(s( "    "
                              , Rd_tag="TEXT"
                              , class=c('Rd_indent', 'Rd_TEXT', 'Rd_tag', 'Rd')
                              )), class= 'Rd')

#' Canonize Rd Text/Code
#'
#' Cannonical text and code are the forms that would come out from reading
#' an Rd file via, <tools::parse_Rd>.
#' Cannonical Rd Text has:
#'
#' * One line per element, with `attr(., 'Rd_tag')=='TEXT'`
#' * the line may not contain a newline within the content but may end the line.
#' * new lines that are in an element by themselves are classed as an 'Rd_newline'.
#'
#' Cannonical Rd code follows the following rules:
#'
#' * One line per line of code.
#' * newline is included at the end of the line string,
#'   not as a separate element.
#' * if there are multiple lines they are bound together in an Rd or Rd_tag list.
#'
Rd_canonize_text <- function(rd){
    assert_that(is(rd, 'Rd'))
    if (length(rd)==0) return(rd)
    if (is.character(rd)) {
        if (!is_exactly(rd, 'Rd_TEXT')) return(rd)
        if (length(rd) > 1L) rd <- forward_attributes(collapse0(rd), rd)
        if (!any(grepl('\\n(?!$)', rd, perl=TRUE))) return(rd)
        rd <- Rd(rd)
    }
    assert_that(is.list(rd))
    if (!Rd_is_all_text(rd)) {
        i <- purrr::map_lgl(rd, is, 'Rd_TEXT')
        if (!any(i))
            return(forward_attributes(lapply(rd, Rd_canonize_text), rd))
        group <- cumsum(abs(!i | c(FALSE, head(!i, -1))))
        is.text <- purrr::map_lgl(split(i, group), all)
        parts <- split(s(rd, class='Rd'), group)

        for (j in seq_along(parts))
            parts[[j]] <- Recall(parts[[j]])

        return(forward_attributes( compact_Rd(parts, FALSE), rd))

    }
    type <- unique(purrr::map_chr(rd, get_attr, 'Rd_tag', ''))
    assert_that(length(type)<=1L)

    lines <- unlist(stringi::stri_split(collapse0(unlist(rd)), regex="(?<=\\n)"))
    lines <- lines[nchar(lines) > 0L]

    if (length(lines) == 1){
        if (lines =='\n') return(.Rd.newline)
        return(forward_attributes(list(Rd_text(lines)), rd))
    }
    lines <- ifelse( lines=='\n'
                   , .Rd.newline
                   , ifelse( is_whitespace(lines)
                           , lapply(lines, structure
                                   , Rd_tag="TEXT"
                                   , class=c('Rd_indent', 'Rd_TEXT', 'Rd_tag', 'Rd'))
                           , lapply(lines, Rd_text, type=type))
                           )
    return(forward_attributes(lines, rd))
}
Rd_canonize_code <- function(rd){
    assert_that(is(rd, 'Rd'))
    if (length(rd)==0) return(rd)
    if (is.character(rd)) {
        if (!is(rd, 'Rd_RCODE')) return(rd)
        if (length(rd) > 1L) rd <- forward_attributes(collapse0(rd), rd)
        if (!any(grepl('\\n(?!$)', rd, perl=TRUE))) return(rd)
        rd <- Rd(rd)
    }
    assert_that(is.list(rd))
    if (!all_inherit(rd, c("Rd_RCODE", "NULL"))) {
        return(forward_attributes( lapply(rd, Rd_canonize_code), rd))
    }
    type <- unique(purrr::map_chr(rd, get_attr, 'Rd_tag', ''))
    assert_that(length(type)==1L)

    lines <- stringi::stri_split(collapse0(unlist(rd)), regex="(?<=\\n)")[[1]]
    lines <- lines[nchar(lines)>0L]
    lines <- ifelse( lines == '\n'
                   , .Rd.code.newline
                   , lapply(lines, Rd_text, type=type)
                   )
    return(forward_attributes(lines, rd))
}
if(FALSE){#@testing
    x <- Rd_usage( .Rd.code.newline
                 , Rd_code('value \\%if\\% proposition'), .Rd.code.newline
                 , Rd_code('value \\%if\\% proposition \\%otherwise\\% alternate'), .Rd.code.newline
                 )
    y <- x[2:3]

    expect_identical( Rd_canonize_code(y)
                    , Rd_usage(Rd_code("value \\%if\\% proposition\n"))
                    )
    expect_identical(Rd_canonize_text(y), y)


    expect_identical( val<-Rd_canonize_code(x)
                    , Rd_usage( .Rd.code.newline
                              , Rd_code("value \\%if\\% proposition\n")
                              , Rd_code("value \\%if\\% proposition \\%otherwise\\% alternate\n")
                              ) )

}

Rd_clean_indent <-
function(indent.with){
    if (is_exactly(indent.with, 'character'))
        indent.with <- Rd(cl(Rd_text(indent.with), 'Rd_indent'))
    if (is_exactly(indent.with, c('Rd_text', 'Rd_code')))
        indent.with <- cl(indent.with, 'Rd_indent')
    if (is(indent.with, 'Rd_indent'))
        indent.with <- Rd(indent.with)
    if (is(indent.with, 'Rd')){
        assert_that( length(indent.with) == 1L)
        if (is_exactly(indent.with[[1L]], 'Rd_text'))
            indent.with[[1L]] <- cl(indent.with[[1L]], 'Rd_indent')
        assert_that(is(indent.with[[1L]], 'Rd_indent'))
        if (grepl('\\t', indent.with))
            doc_warning( type='guidelines_violation'
                         , ._("Tabs are discouraged from being used for indentation" %<<%
                              "as they may not be rendered properly on all possible pagers." %<<%
                              "See https://developer.r-project.org/Rds.html for reference.")
            )
        if (any(grepl('\\n', as.character(indent.with))))
            doc_error( ._("Newlines are not allowed in indent.with"))
        return(indent.with)
    }
    doc_error("bad indent")
}

.Rd_indent <-
function( x, ...
        , indent      = default_("indent"     , FALSE, fun='Rd')
        , indent.with = default_("indent.with", .Rd.default.indent, fun='Rd')
        , indent.first = !is(x, 'Rd_tag')
        ){
    assert_that(is(x, 'Rd'), is.flag(indent))
    if (!indent) return(x)
    indent.with <- Rd_clean_indent(indent.with)
    if (is.character(x)) {
        if (is_exactly(x, 'Rd_TEXT') || is_exactly(x, 'Rd_RCODE') || is_exactly(x, 'Rd_indent'))
            return(forward_attributes(paste0(indent.with, x),x))
        else if (is_exactly(x, 'Rd_newline')) return(x)
        else
            doc_error("Unknown Rd type" %<<% sQuote(collapse(class(x)), '/'))
    }
    assert_that(is.list(x))
    if (!Rd_spans_multiple_lines(x)) return(x)
    parts <- Rd_split(Rd_untag(x))
    if (length(parts) == 1L){
        parts <- lapply( if (indent.first) Rd_untag(x)
                         else tail(Rd_untag(x), -1L)
                       , .Rd_indent
                       , indent=indent
                       , indent.with=indent.with
                       , indent.first = indent.first && !is(x, 'Rd_tag'))
        if (!indent.first)
            parts <- c(unclass(x)[1], parts)
        return(forward_attributes(parts, x))
    }
    for (i in seq_along(parts)) if (Rd_spans_multiple_lines(parts[[i]]))
        parts[[i]] <- .Rd_indent(parts[[i]]
                                , indent=indent
                                , indent.with=indent.with
                                , indent.first = indent.first && !is(x, 'Rd_tag'))
    is.nl <- purrr::map_lgl(parts, is_Rd_newline)
    is.code <- purrr::map_lgl(parts, is, 'Rd_RCODE')
    indent.code <- .Rd(cl(Rd_code(as.character(indent.with)), 'Rd_indent'))
    indents <- ifelse(is.nl, Rd(), ifelse(is.code, indent.code, indent.with))
    if (!indent.first)
        indents[[1]] <- Rd()
    val <- rbind(indents, parts)
    val <- purrr::compact(undim(val))
    val <- compact_Rd(cl(val, 'Rd'))
    val <- forward_attributes(val, x)
    val <- Rd_canonize_text(val)
    val <- Rd_canonize_code(val)
    return(val)
}
if(FALSE){#@testing
    x <- .Rd_strwrap( collapse(stringi::stri_rand_lipsum(3), '\n\n'), wrap.lines = TRUE)
    expect_is(x, 'Rd')
    expect_true(length(x)> 5)

    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'documentation'))
    txt <- Rd_rm_srcref(txt)
    x <- txt[['\\examples']]

    expect_identical(.Rd_indent(x, indent=FALSE), x)

    val <- .Rd_indent(x, indent=TRUE)
    expect_is(val, 'Rd')
    expect_is(val, 'Rd_tag')
    expect_identical( stringi::stri_split_lines1(collapse0(as.character(val)))
                    , stringi::stri_split_lines1(collapse0(as.character(x))) %>%
                        ifelse(. %in% c('\\examples{', '}', ''), ., paste0(space(4), .))
                    )
    expect_equal(length(val), length(x))

    x <- txt[['\\arguments']][[9]]
    val <- .Rd_indent(x, indent=TRUE, indent.with='  ')
    exp <- Rd_item('n', Rd( x[[2]][1:3]
                          , Rd_text(paste0('  ', x[[2]][[4]]))
                          ))
    expect_identical(val, exp)

    x <- Rd_untag(txt[['\\arguments']][8:10])
    val <- .Rd_indent(x, indent=TRUE, indent.with='  ', indent.first=FALSE)
    expect_equal(collapse0(val)
                , "  \\item{n}{number of observations. If \\code{length(n) > 1}, the length" %\%
                  "      is taken to be the number required.}\n" )

    x <- txt[['\\arguments']]
    val <- .Rd_indent(x, indent=TRUE, indent.with='  ')
    expect_equal( collapse0(val)
                , "\\arguments{" %\%
                  "    \\item{x, q}{vector of quantiles.}" %\%
                  "    \\item{p}{vector of probabilities.}" %\%
                  "    \\item{n}{number of observations. If \\code{length(n) > 1}, the length" %\%
                  "      is taken to be the number required.}" %\%
                  "    \\item{mean}{vector of means.}" %\%
                  "    \\item{sd}{vector of standard deviations.}" %\%
                  "    \\item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}" %\%
                  "    \\item{lower.tail}{logical; if TRUE (default), probabilities are" %\%
                  "      \\eqn{P[X \\le x]} otherwise, \\eqn{P[X > x]}.}" %\%
                  "}"
                )
}
if(FALSE){#@testing .Rd_indent specification by options
    x <- Rd_canonize(Rd(Rd_text(c("test strings\n", "second line"))))
    withr::with_options(list( "documentation::Rd::indent" = TRUE
                            , "documentation::Rd::indent.with" = NULL), {
        expect_identical( .Rd_indent(x)
                        , Rd(Rd_text( .Rd.default.indent %<<<% "test strings\n")
                            ,Rd_text( .Rd.default.indent %<<<% "second line")
                            ))
    })
    withr::with_options(list( "documentation::indent" = TRUE), {
        expect_identical( .Rd_indent(x)
                        , Rd( Rd_text( .Rd.default.indent %<<<% "test strings\n")
                            , Rd_text( .Rd.default.indent %<<<% "second line")
                            ))
    })
    withr::with_options(list( "indent" = TRUE), {
        expect_identical( .Rd_indent(x)
                        , Rd( Rd_text( .Rd.default.indent %<<<% "test strings\n")
                            , Rd_text( .Rd.default.indent %<<<% "second line")
                            ))
    })
    withr::with_options(list( "documentation::Rd::indent" = TRUE
                            , "documentation::Rd::indent.with" = "   "), {
        expect_identical( .Rd_indent(x)
                        , Rd( Rd_text( "   test strings\n")
                            , Rd_text( "   second line")
                            ))
    })
    withr::with_options(list( "documentation::Rd::indent" = TRUE
                            , "documentation::Rd::indent.with" = "\t"), {
        expect_warning( .Rd_indent(x)
                      , class= "documentation-warning-guidelines_violation"
                      )
    })
}
if(FALSE){#@testing
    expect_error(.Rd_indent(c('test strings')))

    x <- Rd_usage( .Rd.code.newline
                 , Rd_code('value \\%if\\% proposition\n')
                 , Rd_code('value \\%if\\% proposition \\%otherwise\\% alternate\n')
                 )
    val<- .Rd_indent(x=x, indent=TRUE, indent.with = '  ')
    expect_identical( val
                    , Rd_usage( .Rd.code.newline
                              , Rd_code('  value \\%if\\% proposition\n')
                              , Rd_code('  value \\%if\\% proposition \\%otherwise\\% alternate\n')
                              ))
}


set_option_documentation("documentation::Rd::collapse.lines"
   , description = "should documentation functions return a single " %<<%
                   "string (TRUE) or a array of strings (FALSE) " %<<%
                   "representing the lines of documentation."
   , default = FALSE
   , constraints = list(is.logical)
   )
set_option_documentation("documentation::Rd::collapse.with"
   , description = "when \\code{getOption(documentation::toRd::collapse.lines)}" %<<%
                   "is \\code{TRUE} what the lines should be separated with."
   , default = "\n"
   , constraints = list(~is.string(.))
   )

.Rd_strwrap <-
function( x, ...
        , wrap.lines = default_("wrap.lines", FALSE, fun='Rd')
        , wrap.at    = default_("wrap.at"   , 72L  , fun='Rd')
        ){
    assert_that(is.flag(wrap.lines))
    if (identical(class(x), 'character')) x <- Rd_text(x)
    else assert_that(is(x, 'Rd'))
    if (!wrap.lines) {
        return(x)
    } else
    if (identical(class(x), 'Rd')) {
        assert_that(is.list(x))
        if (Rd_is_all_text(x)) {
            lines <- base::strwrap( collapse0(unlist(x))
                                  , width=wrap.at
                                  , simplify = TRUE)
            if (length(lines) <=1) return(x)
            lines <- lapply(lines, Rd_text, type='TEXT')
            if (length(lines) > 1L && !is.null(attr(x, 'Rd_tag'))) {
                lines <- unname(rbind(lines, .Rd.newline))
                lines <- c(unclass(.Rd.newline), as.vector(lines))
                return(s( lines
                        , class = attr(x, 'class')
                        , Rd_tag = attr(x, 'Rd_tag')
                        , Rd_option = attr(x, 'Rd_option')
                        ))
            } else {
                return(s( unname(rbind(lines, .Rd.newline))
                        , class = attr(x, 'class')
                        , Rd_tag = attr(x, 'Rd_tag')
                        , Rd_option = attr(x, 'Rd_option')
                        ))
            }
        } else {
            return(s( lapply(x, .Rd_strwrap, wrap.lines=wrap.lines, wrap.at=wrap.at)
                    , class  = attr(x, 'class')
                    , Rd_tag = attr(x, 'Rd_tag')
                    , Rd_option = attr(x, 'Rd_option')
                    ))
        }
    } else if (attr(x, 'Rd_tag') =="TEXT"){
        assert_that( is.count(wrap.at)
                   , is.character(x)
                   , is.null(attr(x, 'Rd_option'))
                   )
        # indent <- nchar(.Rd_get_indent(x))

        lines <- base::strwrap(x, width=wrap.at, simplify = TRUE)
        if (length(lines) <=1) return(x)
        lines <- lapply(lines, Rd_text, type='TEXT')
        y <-

        tag <- attr(x, 'Rd_tag') %unless% (attr(x, 'Rd_tag')=='TEXT') %then% 'Rd'
        class <- attr(x, 'class') %unless% (attr(x, 'Rd_tag')=='TEXT') %then% 'Rd'
        return(s( unname(rbind(lines, .Rd.newline))
                , class = 'Rd'
                ))
    } else {
        return(x)
    }
}
if(FALSE){#@testing
    x <- stringi::stri_rand_lipsum(1)

    expect_identical(.Rd_strwrap(x, wrap.lines=FALSE, wrap.at=72L)
                    , Rd_text(x))
    val <- .Rd_strwrap(x, wrap.lines=TRUE , wrap.at=72L)
    expect_is(val, 'Rd')
    expect_true(all_inherit(val, c('Rd_TEXT', 'Rd_newline')))
    expect_equal( unlist(val[1,])
                , base::strwrap(x, 72L)
                )
    expect_true( all(unlist(val[2,])=='\n'))
    val <- .Rd_strwrap(x, wrap.lines=TRUE, wrap.at=50)
    expect_identical( unlist(val[1,])
                    , base::strwrap(x, 50)
                    )
    withr::with_options(list( "documentation::Rd::wrap.lines" = TRUE
                            , "documentation::Rd::wrap.at"    = 50), {
        expect_identical( unlist(.Rd_strwrap(x)[1,])
                        , base::strwrap(x, 50)
                        )
    })

    x <- c("Lorem ipsum", stringi::stri_rand_lipsum(3, start_lipsum = FALSE))
    x <- collapse(x, '\n\n')

    expect_identical( unlist(.Rd_strwrap(x, wrap.lines=TRUE, wrap.at=72L)[1,])
                    , unname(unlist(base::strwrap(x, 72)))
                    )
    expect_equal(sum(.Rd_strwrap(x, wrap.lines=TRUE, wrap.at=72L) == ''), 3L)

    expect_identical( unlist(.Rd_strwrap("   hello\n\nworld", wrap.lines=TRUE, wrap.at=72L)[1,])
                    , c("hello", "", "world")
                    )
}

Rd_canonize <- function(rd, ...){
    if (is_exactly(rd, 'list'))
        rd <- cl(rd, 'Rd')
    else if(is.character(rd) && assert_that(is(rd, 'Rd')))
        rd <- Rd(rd)
    rd <- Rd_canonize_text(rd)
    rd <- Rd_canonize_code(rd)
    rd <- .Rd_strwrap(rd, ...)
    rd <- .Rd_indent(rd, ...)
    return(rd)
}
if(FALSE){#@testing
    rd <- Rd_text("a\nb\nc\n")
    expect_is(rd, 'Rd_TEXT')
    expect_true(is.character(rd))
    expect_length(rd, 1)

    val <- Rd_canonize_text(rd)
    expect_is(rd, 'Rd')
    expect_true(Rd_is_all_text(rd))

    rd <- Rd_examples(Rd( .Rd.code.newline
                        , Rd_code("x<- rnorm(100)\n")
                        , Rd_code("plot(x)\n")))
    expect_identical(Rd_canonize_text(rd), rd)
    expect_identical(Rd_canonize_code(rd), rd)

    Rd_canonize(Rd_canonize_text(rd))

    expect_identical(Rd_canonize_code(Rd_examples(Rd_code("\nx<- rnorm(100)\nplot(x)\n"))), rd)

    rd <- Rd(c( "use the \\backslash to escape."
                 , "and '{}' to group."
                 ))
    val <- Rd_canonize_text(rd)
    expect_is_exactly(val, 'Rd')
    expect_is_exactly(val[[1]], 'Rd_TEXT')
    expect_length(val[[1]], 1L)

    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'documentation'))
    txt <- Rd_rm_srcref(txt)
    expect_identical( Rd_canonize_code(rd <- txt[['\\examples']])
                    , txt[['\\examples']]
                    )

    desc <- txt[['\\description']]
    canonical <- Rd_canonize_text(desc)
    expect_identical( as.character(desc)
                    , as.character(canonical)
                    )

    expect_identical(Rd_canonize_text(txt), txt)
    expect_identical(Rd_canonize_code(txt), txt)
    expect_identical(Rd_canonize(txt), txt)

    x <- Rd_text(c("test strings\n", "second line"))
    val <- Rd_canonize(x)
    expected <- Rd(Rd_text("test strings\n"), Rd_text("second line"))
    expect_identical(val, expected)

    expect_identical(Rd_canonize_text(.Rd.newline), .Rd.newline)


    x <- c( Rd_tag("item"), Rd_text(space(1)), Rd_text("content"))
    expect_identical(Rd_canonize_text(x)[[1]], Rd_tag('item'))
}




# S3 Methods ----------------------------------------------------------


if (FALSE){#@testing toRd,character
    expect_identical( toRd(c("\\hello\n", "%world"))
                    , Rd(Rd_text("\\\\hello\n"), Rd_text("\\%world")))
}

toRd.NULL <- function(obj, ...){Rd()}
if(FALSE){#@testing
    expect_identical(toRd(NULL), Rd())
}

#' @S3method toRd list
toRd.list <- function(obj, ...){
    val <- lapply(obj, toRd, ...)
    val <- compact_Rd(val)
    assert_that( all_inherit(val, 'Rd'))
    cl(val, 'Rd')
}
if(FALSE){#@testing
    l <- list('\\hello ', '%world')
    expect_identical( toRd(l)
                    , s( list( Rd_text("\\\\hello ")
                             , Rd_text("\\%world"))
                       , class='Rd')
                    )

    l <- list( first  = Rd("first text")
             , second = Rd(c("second", "text"))
             , third = NULL
             )

    val <- toRd(l)

    expect_is(val, 'Rd')
    expect_is(val[[1]], 'Rd_TEXT')
}



#' @export
#' @S3method toRd Rd
toRd.Rd <- function(obj, ...){
    assert_that( is(obj, 'Rd')
               , is.character(obj)  %if% is(obj, c('Rd_TEXT', 'Rd_RCODE'))
                 %otherwise% is.list(obj)
               )
    obj
}
if(FALSE){#@testing
    obj <- Rd("test")
    expect_identical(toRd(obj), obj)
    expect_error(toRd(cl(TRUE, 'Rd')))

    obj <- Rd("\\rd")
    expect_identical(toRd(obj), obj)
}



#' @S3method toRd person
toRd.person<-
function( obj
        , ...
        , include = c('given', 'family', 'email')
        ){
    comma_list(format( obj, include = include
                     , braces  = list(email = c('\\email{', '}'))
                     ))
}
if (FALSE) {#@testing toRd,author
    obj <- list(author = c( person('Andrew', 'Redd'
                                  , email='andrew.redd@hsc.utah.edu')
                          , person('Drew'  , 'Blue')
                          ))
    val <- toRd(obj)
    expect_is_exactly(val, 'Rd')
    expect_length(val, 1L)
    expect_is_exactly(val[[1]], 'Rd_TEXT')
    expect_length(val[[1]], 1L)
    expect_identical( as.character(toRd(obj))
                    , "Andrew Redd \\email{andrew.redd@hsc.utah.edu} and Drew Blue"
                    )
}
if(FALSE){#! @testing
    object <-c( person('First' , 'Author', email='me1@email.com')
              , person('Second', 'Author', email='me2@email.com')
              )
    val <- toRd(object)
    expect_is_exactly(val, 'Rd')
    expect_length(val, 1L)
    expect_is_exactly(val[[1L]], 'Rd_TEXT')
    expect_length(val[[1]], 1L)

    expect_equal( as.character(val)
                , 'First Author \\email{me1@email.com}' %<<% 'and' %<<%
                  'Second Author \\email{me2@email.com}'
                  )
    expect_equal( toRd( c( person('Andrew', 'Redd', email='andrew.redd@hsc.utah.edu')
                         , person('Drew'  , 'Blue')
                         ) )
                , s( list( s('Andrew Redd \\email{andrew.redd@hsc.utah.edu}' %<<%
                             'and Drew Blue'
                            , Rd_tag = 'TEXT'
                            , class = c('Rd_TEXT', 'Rd_tag', 'Rd')))
                           , class='Rd')
                )
}

### bibstyle('documentation') #####
# setHook( packageEvent('documentation', event = "onLoad")
#        , function(...){
#             if (requireNamespace("tools", quietly = TRUE))
#                 tools::bibstyle('documentation', collapse = collapse, .init=TRUE)
#        }
#        )
# if(FALSE){#!@testing documentation bibstyle
#     expect_true("documentation" %in% tools::getBibstyle())
#
#     object <- citation() %>% structure(class='bibentry')
#     default.style <- toRd(object, style='JSS')
#     doc.style     <- toRd(object, style='documentation')
#
#     expect_true(default.style != doc.style)
# }

### toRd,Documentation-Keyword #####
setMethod('toRd', 'Documentation-Keyword', function( obj, ...){
    if(length(obj) == 1 ) Rd_keyword(obj@.Data)
    cl(lapply(obj@.Data, Rd_keyword), 'Rd')
})
if(FALSE){#! @testing
    obj <- new('Documentation-Keyword', 'utilities')
    val <- toRd(obj)
    expect_is_exactly(val, 'Rd')
    expect_is_exactly(val[[1]], 'Rd_tag')
    expect_true(is_Rd_tag(val[[1]], '\\keyword'))

    obj <- new('Documentation-Keyword', c('utilities', 'character'))
    val <- toRd(obj)
    expect_is_exactly(val, 'Rd')
    expect_length(val, 2)
    expect_is_exactly(val[[1]], 'Rd_tag')
    expect_true(is_Rd_tag(val[[1]], '\\keyword'))
    expect_equal( collapse0(as.character(val))
                , '\\keyword{utilities}\\keyword{character}')
}


### toRd,FormattedText/Rd #####
setMethod('toRd', 'FormattedText/Rd',
function( obj, ...){
    #! Convert formatted text into Rd lines.
    s(S3Part(obj, strictS3 =TRUE), class='Rd')
})
if(FALSE){#! @testing
    obj <- FT_Rd( Rd_text("A description of ")
                , Rd_tag('code', Rd_tag('link', Rd_code("toRd")))
                , .Rd.newline
                )
    expect_is(obj, 'FormattedText/Rd')
    val <- toRd(obj)
    expect_is_exactly(val, 'Rd')
    expect_identical(val
                    , Rd( Rd_text("A description of ")
                        , Rd_tag('code', Rd_tag('link', Rd_code("toRd")))
                        , .Rd.newline
                        ))
    obj <- FT_Rd('Hello world!')
    expect_identical( toRd(obj), Rd('Hello world!'))
    expect_false(identical(toRd(obj), obj))
}

### toRd,FormattedText/character #####
setMethod('toRd', 'FormattedText/character',
function(obj, ...){
    txt <- S3Part(obj, strictS3 =TRUE)
    if (length(txt)==0L) return(Rd())
    if (length(txt)==1L) return(toRd(txt))
    paragraphs <- lapply(txt, toRd, ...)
    paragraphs <- compact_Rd(paragraphs)
    val <- cl(unname(head(undim(rbind(paragraphs, .Rd.break)), -1L)), 'Rd')
    val <- .Rd_strwrap(val, ...)
    Rd_canonize(val)
})
if(FALSE){#@testing
    obj <- FormattedText(stringi::stri_rand_lipsum(3))
    as.rd <- toRd(obj)
    expect_length(as.rd, 5 )
    expect_length(as.character(as.rd), 5 )
    expect_is_exactly(as.rd, 'Rd')

    expect_true(all(as.rd[c(2,4)]=='\n'))

    expect_identical(toRd(FT()), Rd())

    wrapped <- toRd(obj, wrap.lines=TRUE, wrap.at=50)
    expect_true(length(as.character(wrapped)) > 5L)
    expect_identical(wrapped, Rd_canonize(wrapped))
}
