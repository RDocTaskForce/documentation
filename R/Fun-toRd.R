#' @include Classes.R
#' @include Fun-default.R

.valid_Rd <- function(object){
    if (inherits(object, 'Rd')) return(TRUE)
    if (is.character(object)) return(TRUE)
    else if (is.list(object)){
        if (length(object) == 0) return(TRUE)
        if (all( sapply(object, is.character)
               | sapply(object, is.null)
               )) return(TRUE)
        else deparse(substitute(object)) %<<%
            ._("is a list but not all elements are characters.")
    } else deparse(substitute(x)) %<<%
        ._("is neither character nor a list of characters.")
}

Rd <-
function( x
        , ... #< ignored.
        , collapse.lines  = default(collapse.lines, FALSE)
        , collapse.with   = default(collapse.with , "\n" )
        , wrap.lines      = default(wrap.lines    , FALSE)
        , wrap.at         = default(wrap.lines    , 72L  )
        ){
    if (inherits(x, 'Rd')) return(x)
    assert_that(.valid_Rd(x))
    if (is.list(x)) return(s(unlist(lapply(x, Rd)), class='Rd'))

    if (collapse.lines && wrap.lines)
        doc_warning(._("Options 'collapse.lines' and 'wrap.lines'" %<<%
                       "should not both be set as the combination is" %<<%
                       "likely to produce undersireable effects."))

    x <- clean_Rd(x)
    x <- .Rd_strwrap(x, wrap.lines, wrap.at)
    x <- .Rd_collapse(x, collapse.lines, collapse.with)
    s(x, class='Rd')
}
if(FALSE){
    a <- "test"
    b <- Rd(a)
    expect_is(b, 'Rd')

    a <- stringi::stri_rand_lipsum(3)
    b <- Rd(a, wrap.lines=TRUE)
    expect_true(length(b)>3)

    c <- Rd(a, wrap.lines=FALSE)
    expect_is(c, 'Rd')
    d <- Rd(c, wrap.lines=TRUE)
    expect_is(d, 'Rd')
    expect_identical(c, d)

    expect_warning(b <- Rd(a, wrap.lines = TRUE, collapse.lines = TRUE)
                  , class="documentation-warning" )
    expect_length(b, 1)

    d <- Rd(c <- .Rd_indent(b, TRUE, with='        ')
           , wrap.lines = TRUE, collapse.lines = FALSE) %>% unclass
}



#' @export
#' @importFrom tools toRd
# setGeneric('toRd', tools::toRd, valueClass=c('Rd', 'character', 'list'))
setGeneric('toRd', def =
function(obj, ...){
    ans <- standardGeneric("toRd")
    if (is.list(ans)){
        if (any(null <- sapply(ans, is.null)))
            ans <- ans[!null]
        if (!all( sapply(ans, is.character)))
            doc_error(._( "Method of generic function %1$s for class %2$s"%<<%
                          "returned a list, but not of character vectors." %<<%
                          "Methods of %1$s are expected to retun a %3$s vector."
                        , sQuote("toRd"), dQuote(class(obj)), 'character')
                     , type="toRd-invalid_result" )
        ans <- sapply(ans, collapse_nl)
    }
    if (is.character(ans))
        return (s(ans, class='Rd'))
    else
        doc_error(._( "Method of generic function %1$s for class %2$s" %<<%
                      "returned a %4$s." %<<%
                      "Methods of %1$s are expected to retun a %3$s vector."
                    , sQuote("toRd"), dQuote(class(obj))
                    , 'character', dQuote(class(ans))
                    ))
})
toRd@valueClass <- 'Rd'
if(FALSE){
    val <- toRd('character')
    expect_identical(val, Rd("character"))
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
.Rd_indent <-
function( x, ...
        , indent      = default_("indent"     , FALSE, fun='Rd')
        , indent.with = default_("indent.with", '  ' , fun='Rd')
        ){
    if (!indent) return(x)
    assert_that(is.string(indent.with))
    if (grepl('\\t', indent.with))
        doc_warning(type='guidelines_violation'
                   , ._("Tabs are discouraged from being used for indentation" %<<%
                        "as they may not be rendered properly on all possible pagers." %<<%
                        "See https://developer.r-project.org/Rds.html for reference.")
                   )
    if (any(grepl('\\n', x)))
        doc_warning( ._("Newlines detected for indentation;" %<<%
                        "Not all lines may be indented.")
                   )
    s(paste0(indent.with, x), class=attr(x, 'class'))
}
if(FALSE){#@testing
    x <- c("test strings", "second line")

    expect_identical(.Rd_indent(c("test strings", "second line"))
                    , c("test strings", "second line")
                    )

    expect_identical(.Rd_indent(c("test strings", "second line"), indent=TRUE)
                    , c("  test strings", "  second line"))

    withr::with_options(list( "documentation::Rd::indent" = TRUE
                            , "documentation::Rd::indent.with" = NULL), {
        expect_identical(.Rd_indent(c("test strings", "second line"))
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
.Rd_collapse <-
function( x, ...
        , collapse.lines  = default_("collapse.lines", FALSE, fun='Rd')
        , collapse.with   = default_("collapse.with" , "\n" , fun='Rd')
        ){
    assert_that(is.flag(collapse.lines))
    if (!collapse.lines) return(x)

    assert_that(is.string(collapse.with))
    s(collapse(x, with = collapse.with), class=attr(x, 'class'))
}

.Rd_get_indent <- function(x){
    gsub("^( *)[^ ].*$", "\\1", x[[1]])
}
if(FALSE){
    x <- "   hello world"
    expect_equal(.Rd_get_indent(x), "   ")
    expect_equal(.Rd_get_indent("hello world"), "")
}
.Rd_strwrap <-
function( x, ...
        , wrap.lines = default_("wrap.lines", FALSE, fun='Rd')
        , wrap.at    = default_("wrap.at"   , 72L  , fun='Rd')
        ){
    assert_that(is.flag(wrap.lines))
    if (!wrap.lines) return(x)
    assert_that(is.count(wrap.at))
    indent <- nchar(.Rd_get_indent(x))
    s( base::strwrap(x, wrap.at, indent=indent, exdent=indent)
     , class=attr(x, 'class'))
}
if(FALSE){#@testing
    x <- stringi::stri_rand_lipsum(1)

    expect_identical(.Rd_strwrap(x, wrap.lines=FALSE, wrap.at=72L), x)
    expect_identical(.Rd_strwrap(x, wrap.lines=TRUE , wrap.at=72L)
                    , base::strwrap(x, 72L)
                    )
    expect_identical(.Rd_strwrap(x, wrap.lines=TRUE, wrap.at=50)
                    , base::strwrap(x, 50)
                    )
    withr::with_options(list( "documentation::Rd::wrap.lines" = TRUE
                            , "documentation::Rd::wrap.at"    = 50), {
        expect_identical( .Rd_strwrap(x)
                        , base::strwrap(x, 50)
                        )
    })

    x <- c("Lorem ipsum"
          , interleave( rep('', 3)
                      , stringi::stri_rand_lipsum(3, start_lipsum = FALSE)
                      )
          )
    expect_identical( .Rd_strwrap(x, wrap.lines=TRUE, wrap.at=72L)
                    , unname(unlist(sapply(x, base::strwrap, 72)))
                    )
    expect_equal(sum(.Rd_strwrap(x, wrap.lines=TRUE, wrap.at=72L) == ''), 3L)


    expect_identical( .Rd_strwrap("   hello\n\nworld", wrap.lines=TRUE, wrap.at=72L)
                    , c("   hello", "", "   world")
                    )
}

Rd_tag  <-
function( content
        , name=deparse(substitute(content))
        , opt=character(0)
        , ...
        ){
    assert_that( is.string(name))
    if (length(content)==0) return(character(0))
    if (!inherits(content, 'Rd'))
        content <- toRd(content, ...)
    if (length(opt)>0L)
        name <- name %<<<% '[' %<<<% comma_list(opt) %<<<% ']'
    if (length(content) == 1)
        s( sprintf("\\%s{%s}", name, content), class=c('Rd_tag', 'Rd'))
    else
        s(.Rd_collapse(c( sprintf("\\%s{", name)
                        , .Rd_indent(content, ...)
                        , "}"), ...), class=c('Rd_tag', 'Rd'))
}
if(FALSE){#! @testing
    expect_error(Rd_tag('test', NULL), "name is not a string")
    expect_error(Rd_tag('test', c('a', 'b')), "name is not a string")
    expect_error(Rd_tag('test', 1), "name is not a string")
    expect_is(Rd_tag('my name', 'name'), "Rd_tag")
    expect_is(Rd_tag('my name', 'name'), "Rd")
    expect_equal( unclass(Rd_tag('my name', 'name')), "\\name{my name}")
    expect_equal( unclass(Rd_tag(c('line1', 'line2'), 'name'))
                , c('\\name{', 'line1', 'line2', '}')
                )
    name <- 'testing'
    expect_equal(unclass(Rd_tag(name)), '\\name{testing}')

    obj <- new('FormattedText', stringi::stri_rand_lipsum(3))
    as.tag <- Rd_tag(obj)
    expect_is(as.tag, 'Rd_tag')
    expect_length(as.tag, 7)
    expect_identical(as.tag[c(1,7)], c('\\obj{', '}'))

    val <- Rd_tag('dest', 'link', opt='pkg')
    expect_is(val, 'Rd')
    expect_identical(unclass(val), "\\link[pkg]{dest}")
}

#' @export
setMethod('toRd', 'person',
function( obj
        , ...
        , include = c('given', 'family', 'email')
        ){
    comma_list(format( obj, include = include
                     , braces  = list(email = c('\\email{', '}'))
                     ))
})
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

tools::bibstyle('documentation', collapse = collapse, .init=TRUE)
if(FALSE){#!@testing documentation bibstyle
    object <- citation() %>% structure(class='bibentry')
    default.style <- toRd(object, style='JSS')
    doc.style     <- toRd(object, style='documentation')

    expect_true(default.style != doc.style)
}


setMethod('toRd', 'Documentation-Keyword', function( obj, ...)sprintf("\\keyword{%s}", obj@.Data))
if(FALSE){#! @testing
    obj <- new('Documentation-Keyword', c('utilities', 'character'))
    val <- toRd(obj)
    expect_is(val, 'Rd')
    expect_equal( unclass(val)
                , c('\\keyword{utilities}', '\\keyword{character}'))
}

setMethod('toRd', 'FormattedText',
function( obj
        , ...
        , add.blank.lines = TRUE
        ){
    #! Convert formatted text into Rd lines.
    #!
    #! \\note{ Assumes that each element of the text is a paragraph.)
    if( length(obj) == 0) return(character(0))
    else if( length(obj) == 1) return(obj@.Data)
    else return( utils::head(interleave(obj, rep('', length(obj))),-1) )

})
if(FALSE){#! @testing
    obj <- FormattedText()
    expect_identical(toRd(obj), Rd(character(0)))

    obj <- FormattedText('Hello world!')
    expect_identical(toRd(obj), Rd('Hello world!'))
    expect_false(identical(toRd(obj), obj))

    obj <- FormattedText(stringi::stri_rand_lipsum(3))
    as.rd <- toRd(obj)
    expect_equal(length(as.rd), 5 )
    expect_is(as.rd, 'Rd')
    expect_identical(mode(as.rd), 'character')
}


#' @export
# setMethod('toRd', 'vector',
# function(obj, name=deparse(substitute(obj)), ...){
#     if (length(obj) == 0) return(character(0))
#     if(is.atomic(obj)) Rd_tag(NextMethod(toRd, obj), name=name)
#     Rd_tag(sapply(obj, toRd), name=name)
# })
if(FALSE){#@testing
    obj <- new('FormattedText', txt <- stringi::stri_rand_lipsum(3))
    expect_is(obj, 'vector')
    as.rd <- toRd(obj, 'description')
    expect_equal(unclass(as.rd), Rd(txt))
}

