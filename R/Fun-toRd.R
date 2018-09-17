#' @include Classes.R
#' @include Fun-default.R
#' @importFrom tools toRd



### toRd #######################################################################
#' @export
# setGeneric('toRd', tools::toRd, valueClass=c('Rd', 'character', 'list'))
setGeneric('toRd',
function(obj, ...){
    ans <- standardGeneric("toRd")
    if (is_exactly(ans, 'Rd')) return(ans)
    if (is(ans, 'Rd')) return(s(list(ans), class='Rd'))
    if (is.character(ans)) return(Rd(ans))
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
                    , Rd(Rd_text(c( "use the \\\\backslash to escape."
                                  , "and '\\{\\}' to group."
                                  ))))
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

Rd_canonize_text <- function(rd){
    assert_that(is(rd, 'Rd'))
    if (!Rd_is_all_text(rd))
        return(forward_attributes( lapply(Rd_canonize_text),.rd))
    if (is(rd, 'Rd_TEXT'))
        rd <- Rd(rd)
    if (is.list(rd)){
        type <- unique(purrr::map_chr(rd, get_attr, 'Rd_tag', ''))
        assert_that(length(type)==1L)
    } else
        type <- get_attr(rd, 'Rd_tag')

    lines <- stringi::stri_split_lines1(collapse0(unlist(rd)))

    if (length(lines) <=1) return(x)
    lines <- ifelse(lines=='', list(NULL), lapply(lines, Rd_text, type=type))

    lines <- cbind( lines
                  , c( .Rd.newline %if% Rd_starts_with_newline(rd)
                     , rep(.Rd.newline, length(lines)-2L)
                     , .Rd.newline %if% Rd_ends_with_newline(rd)
                     ))
    return(forward_attributes(compact_Rd(lines), rd))
}
if(FALSE){#@testing
    rd <- Rd_text("a\nb\nc\n")
    expect_is(rd, 'Rd_TEXT')
    expect_true(is.character(rd))
    expect_length(rd, 1)

    val <- Rd_canonize_text(rd)

}

Rd_clean_indent <-
function(indent.with){
    if (is_exactly(indent.with, 'character'))
        return(Rd(cl(Rd_text(indent.with), 'Rd_indent')))
    if (is_exactly(indent.with, c('Rd_text', 'Rd_code')))
        indent.with <- cl(indent.with, 'Rd_indent')
    if (is(indent.with, 'Rd_indent'))
        return(Rd(indent.with))
    if (is(indent.with, 'Rd')){
        assert_that( length(indent.with) == 1L)
        if (is_exactly(indent.with[[1L]], 'Rd_text'))
            indent.with[[1L]] <- cl(indent.with[[1L]], 'Rd_indent')
        assert_that(is(indent.with[[1L]], 'Rd_indent'))
        return(indent.with)
    }
    doc_error("bad indent")
}

.Rd_indent <-
function( x, ...
        , indent      = default_("indent"     , FALSE, fun='Rd')
        , indent.with = default_("indent.with", .Rd.default.indent, fun='Rd')
        ){
    assert_that(is(x, 'Rd'), is.flag(indent))
    if (!indent) return(x)
    indent.with <- Rd_clean_indent(indent.with)
    if (grepl('\\t', indent.with))
        doc_warning(type='guidelines_violation'
                   , ._("Tabs are discouraged from being used for indentation" %<<%
                        "as they may not be rendered properly on all possible pagers." %<<%
                        "See https://developer.r-project.org/Rds.html for reference.")
                   )
    if (any(grepl('\\n.', as.character(x))))
        doc_warning( ._("Newlines detected for indentation;" %<<%
                        "Not all lines may be indented.")
                   )
    if (is.character(x))
        return(s( list(indent.with, x)
                , class = 'Rd'
                ))
    if (is.list(x)) {
        parts <- Rd_split(x)
        p2 <- lapply(parts, function(x){
            .Rd_indent(x) %if% Rd_spans_multiple_lines(x) %otherwise% x
        })
        indents <- rep(indent.with, length(parts))
        if (length(parts[[1]])==1 && is.character(parts[[1]]) && parts[[1]] == '\n') {
            indents[1] <- list(NULL)
        }
        return(s( compact_Rd(rbind(indents, p2))
                , dim=NULL, dimnames=NULL
                , Rd_tag     = attr(x, 'Rd_tag')
                , Rd_options = attr(x, 'Rd_options')
                , class      = attr(x, 'class')
                ))
    }
}
if(FALSE){#@testing
    x <- .Rd_strwrap( collapse(stringi::stri_rand_lipsum(3), '\n\n'), wrap.lines = TRUE)
    expect_is(x, 'Rd')
    expect_true(length(x)> 5)

    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'documentation'))
    x <- txt[['\\examples']]

    expect_identical(.Rd_indent(x, indent=FALSE), x)

    val <- .Rd_indent(x, indent=TRUE)
    expect_is(val, 'Rd')
    expect_is(val, 'Rd_tag')


    expect_error(.Rd_indent(c("test strings", "second line")))

    withr::with_options(list( "documentation::Rd::indent" = TRUE
                            , "documentation::Rd::indent.with" = NULL), {
        expect_identical(.Rd_indent(Rd(c("test strings\n", "second line")), indent=TRUE)
                        , c("  test strings", "  second line"))
    })
    withr::with_options(list( "documentation::indent" = TRUE), {
        expect_identical(.Rd_indent(c("test strings", "second line"))
                        , c("  test strings", "  second line"))
    })
    withr::with_options(list( "indent" = TRUE), {
        expect_identical(.Rd_indent(c("test strings", "second line"))
                        , c("  test strings", "  second line"))
    })
    withr::with_options(list( "documentation::Rd::indent" = TRUE
                            , "documentation::Rd::indent.with" = "    "), {
        expect_identical(.Rd_indent(c("test strings", "second line"))
                        , c("    test strings", "    second line"))
    })
    withr::with_options(list( "documentation::Rd::indent" = TRUE
                            , "documentation::Rd::indent.with" = "\t"), {
        expect_warning(.Rd_indent(c("test strings", "second line"))
                      , class= "documentation-warning-guidelines_violation"
                      )
    })
    expect_warning(.Rd_indent(collapse_nl(x), indent=TRUE)
                  , class = "documentation-warning" )


    expect_identical( .Rd_indent(c( "a", "", "b"), indent=TRUE, indent.with = "  ")
                    , c("  a", "", "  b"))

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


Rd_count_lines <-
function(x){
    sum(. <- unlist(stringr::str_split(x, '')) == '\n') +
        (length(x) > 1) -
        (tail(.,1))
}
if(FALSE){#@testing
    expect_equal(Rd_count_lines(.Rd.newline), 1L)

    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'documentation'))

    expect_equal(Rd_count_lines(x <- txt[1:8]), 4L)
}



# S3 Methods ----------------------------------------------------------

#' @S3method toRd list
toRd.list <- function(obj, ...){
    val <- lapply(obj, toRd)
    assert_that( all(purrr::map_lgl(val, inherits, 'Rd'))
               , all(purrr::map_lgl(val, is.character))
               )
    if (!is.null(names(obj))){
        lens <- purrr::map_int(val, length)
        new.names <- rep(names(obj), lens)
        s(unlist(val), names=new.names)
    } else return(unlist(val))
}
if(FALSE){#@testing
    l <- list('\\hello', '%world')
    expect_identical( toRd(l)
                    , s( c("\\\\hello", "\\%world")
                       , class='Rd')
                    )

    l <- list( first = Rd("first text")
             , second = Rd(c("second", "text"))
             , third = NULL
             )

    val <- toRd(l)

    expect_is(val, 'Rd')
    expect_true(is.character(val))
    expect_equal(unclass(val)
                , c( first  = "first text"
                   , second = "second"
                   , second = "text"
                   )
                )

    obj <- list(author = c( person('Andrew', 'Redd'
                                  , email='andrew.redd@hsc.utah.edu')
                          , person('Drew'  , 'Blue')
                          ))
    expect_identical( unclass(toRd(obj))
                    , c(author ="Andrew Redd \\email{andrew.redd@hsc.utah.edu} and Drew Blue")
                    )
}


#' @export
#' @S3method toRd Rd
toRd.Rd <- function(obj, ...){
    assert_that(is.character(obj) && inherits(obj, 'Rd'))
    obj
}
if(FALSE){#@testing
    obj <- Rd("test")
    expect_identical(toRd(obj), obj)
    expect_error(toRd(cl(TRUE, 'Rd')))

    obj <- Rd("\\rd")
    expect_identical(toRd(obj), obj)

    selectMethod('toRd', class(obj))
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
if(FALSE){#! @testing
    object <- person('Andrew', 'Redd', email='andrew.redd@hsc.utah.edu')
    val <- toRd(object)
    expect_is(val, 'Rd')
    expect_equal(unclass(val), 'Andrew Redd \\email{andrew.redd@hsc.utah.edu}')

    object <-c( person('First' , 'Author', email='me1@email.com')
              , person('Second', 'Author', email='me2@email.com')
              )
    val <- toRd(object)
    expect_is(val, 'Rd')
    expect_equal( unclass(val)
                ,
                  'First Author \\email{me1@email.com}' %<<% 'and' %<<%
                  'Second Author \\email{me2@email.com}'
                  )
    expect_equal( toRd( c( person('Andrew', 'Redd', email='andrew.redd@hsc.utah.edu')
                         , person('Drew'  , 'Blue')
                         ) )
                , structure( 'Andrew Redd \\email{andrew.redd@hsc.utah.edu}' %<<%
                             'and Drew Blue'
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
setMethod('toRd', 'Documentation-Keyword', function( obj, ...)sprintf("\\keyword{%s}", obj@.Data))
if(FALSE){#! @testing
    obj <- new('Documentation-Keyword', c('utilities', 'character'))
    val <- toRd(obj)
    expect_is(val, 'Rd')
    expect_equal( unclass(val)
                , c('\\keyword{utilities}', '\\keyword{character}'))
}


### toRd,FormattedText/Rd #####
setMethod('toRd', 'FormattedText/Rd',
function( obj, ...){
    #! Convert formatted text into Rd lines.
    S3Part(obj, strictS3 =TRUE)
})
if(FALSE){#! @testing
    obj <- FT_Rd()
    expect_is(obj, 'FormattedText/Rd')
    val <- toRd(obj)
    attr(class(val), 'package') <- NULL
    expect_identical(val, Rd(character(0)))

    obj <- FT_Rd('Hello world!')
    expect_identical(unclass(toRd(obj)), 'Hello world!')
    expect_false(identical(toRd(obj), obj))
}

### toRd,FormattedText/character #####
setMethod('toRd', 'FormattedText/character',
function(obj, ...){
    txt <- S3Part(obj, strictS3 =TRUE)
    if (length(txt)==0L) return(Rd(character(0)))
    if (length(txt)==1L) return(toRd(txt))
        paragraphs <- lapply(txt, toRd, ...)
    cl( flatten_lines(paragraphs), 'Rd')
})
if(FALSE){#@testing
    obj <- FormattedText(stringi::stri_rand_lipsum(3))
    as.rd <- toRd(obj)
    expect_equal(length(as.rd), 5 )
    expect_is(as.rd, 'Rd')
    expect_identical(mode(as.rd), 'character')

    expect_true(all(as.rd[c(2,4)]==''))

    expect_identical(toRd(FT()), Rd(character(0)))
}
