#' @include Classes.R
#' @include Fun-default.R

setOldClass('Rd')
#' @export
Rd <-
function( x
        , ... #< ignored.
        , collapse.lines  = default(collapse.lines, FALSE)
        , collapse.with   = default(collapse.with , "\n" )
        , wrap.lines      = default(wrap.lines    , FALSE)
        , wrap.at         = default(wrap.at       , 72L  )
        ){
    if (inherits(x, 'Rd')) return(x)
    if (is.list(x)){
        l <- lapply(x, Rd)
        return(s(unlist(l), class='Rd'))
    }
    assert_that(is.character(x))
    if (collapse.lines && wrap.lines)
        doc_warning(._("Options 'collapse.lines' and 'wrap.lines'" %<<%
                       "should not both be set as the combination is" %<<%
                       "likely to produce undersireable effects."))

    x <- .Rd_strwrap(x, wrap.lines=wrap.lines, wrap.at=wrap.at)
    x <- .Rd_collapse(x, collapse.lines=collapse.lines, collapse.with=collapse.with)
    s(x, class='Rd')
}
if(FALSE){#@testing
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

    expect_error(Rd(NULL))


    val <- Rd(c(a="1", b="2"))
    expect_equal(names(val), c('a','b'))
}


### toRd #######################################################################
#' @export
#' @importFrom tools toRd
# setGeneric('toRd', tools::toRd, valueClass=c('Rd', 'character', 'list'))
setGeneric('toRd', def =
function(obj, ...){
    ans <- standardGeneric("toRd")
    if (is.character(ans))
        return(Rd(ans))
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

    val <- toRd(c( "use the \\backslash to escape."
                 , "and '{}' to group."
                 ))
    expect_is(val, 'Rd')
    expect_identical(unclass(val)
                    , c( "use the \\\\backslash to escape."
                       , "and '\\{\\}' to group."
                       ) )
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
if(FALSE){#@testing
    expect_identical( .Rd_collapse(c("hello", "world"), collapse.lines=TRUE, collapse.with="\xE1")
                    , "hello\xE1world")

    x <- Rd(c("hello", "world"))
    expect_identical(.Rd_collapse(x, collapse.lines=TRUE, collapse.with="\xE1")
                    , Rd("hello\xE1world"))
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

    obj <-
        c( person('Andrew', 'Redd', email='andrew.redd@hsc.utah.edu')
                                  , person('Drew'  , 'Blue')
                                  )
    expect_identical( unclass(toRd(obj))
                    , "Andrew Redd \\email{andrew.redd@hsc.utah.edu} and Drew Blue"
                    )
}

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

### toRd,FormattedText #####
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

