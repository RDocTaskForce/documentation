#' @include utils.R

#TODO
#'
#' @export
setClass('FormattedText', contains="VIRTUAL")

#' @export
FT_html <- setClass("FormattedText/html", list('FormattedText', 'shiny.tag'))
if(FALSE){#@testing FormattedText/html
    html <- with(htmltools::tags, p("html formatted text"))
    expect_is(html, 'shiny.tag')
    obj <- FT_html(html)
    expect_is(obj, 'shiny.tag')
    expect_is(obj, 'FormattedText/html')
    expect_is(obj, 'FormattedText')
}

#' @export
FT_Rd <- setClass("FormattedText/Rd", list('FormattedText', 'Rd'))
#' @export
setMethod('initialize', "FormattedText/Rd",
function( .Object, value=list(), ...){
    if (is.character(value))
        value <- Rd_text(value)
    if (is(value, 'Rd') && !is_exactly(value, 'Rd'))
        value <- Rd(value)
    S3Part(.Object) <- Rd_canonize(value, ...)
    .Object
})
setValidity('FormattedText/Rd', function(object){
    assert_that(inherits(S3Part(object, strictS3 = TRUE), 'Rd'))
})
if(FALSE){#@testing FormattedText/Rd
    x <- Rd_tag("note", Rd_text("Rd format text"))
    obj <- FT_Rd(x)
    expect_is(obj, 'Rd')
    expect_is(obj, 'FormattedText/Rd')
    expect_is(obj, 'FormattedText')

    y <- S3Part(x, strictS3 = TRUE)
    expect_identical(x, y)

    control = list(wrap.lines = TRUE, wrap.at=72)
    description <- withr::with_seed(20180921, stringi::stri_rand_lipsum(3))
    description <- Rd_canonize( Rd(collapse(description, '\n\n')), control=control)
    expect_is(description, 'Rd')
    expect_true(length(description)>5L)

    x <- FT_Rd(Rd(description))
    z <- S3Part(x, strictS3 = TRUE)
    attr(class(z), 'package') <- NULL
    expect_identical(z, description)

    expect_error(val <- S3Part(FT_Rd(1L)))
}

#' Plain Text
#'
#' Plain text can be used for documentation when no special formatting is needed.
#' Use \code{FT} to create plain text descriptors.
#'
#' @note Since no special formatting is implied elements of text are assumed to be
#' separate paragraphs, and as such extra blank lines are inserted when converting to
#' Rd.  Likewise each element will be wrapped in a \code{<p>} tag for
#' conversion to HTML.
#'
#' @export
FT_character <- setClass("FormattedText/character", list('FormattedText', 'character'))
setMethod('initialize', "FormattedText/character",
function( .Object, value=character(0), ...){
    if (!is.character(value))
      value <- as.character(value)
    assert_that(is.character(value))
    S3Part(.Object) <- value
    .Object
})
setValidity('FormattedText/character', function(object){
    assert_that(inherits(S3Part(object, strictS3 = TRUE), 'character'))
})
if(FALSE){#@testing FormattedText/character
    x <- "just plain text"
    obj <- FT_character(x)
    expect_is(obj, 'character')
    expect_is(obj, 'FormattedText')
    expect_is(obj, 'FormattedText/character')
    expect_identical(S3Part(obj, strictS3 = TRUE), "just plain text")

    obj <- FT_character()
    expect_identical(S3Part(obj, strictS3 = TRUE), character(0))

    val <- S3Part(FT_character(1L), strictS3 = TRUE)
    expect_equal(val, '1')
}


setAs('character', 'FormattedText', function(from)FT_character(from))
setAs('shiny.tag', 'FormattedText', function(from)FT_html(from))
setAs('Rd'       , 'FormattedText', function(from)FT_Rd(from))
setAs('NULL'     , 'FormattedText', function(from)FT())

FormattedText <- FT <- function(from=character(0))as(from, 'FormattedText')

if(FALSE){
    expect_is(as("hello world", 'FormattedText'), 'FormattedText/character')

        y <- with(htmltools::tags, div('Regular', a('link')))
    expect_is(y, 'shiny.tag')
    expect_is(as(y, 'FormattedText'), 'FormattedText/html')
}

setVector( element = "FormattedText"
         , Class   = "SectionList"
         )

