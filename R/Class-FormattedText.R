#' @include utils.R

### Class: FormattedText(VIRTUAL) #####
#'
#' @export
setClass('FormattedText', contains="VIRTUAL")

### Class: FormattedText/html #####
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

### Class: FormattedText/Rd #####
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

### Class: FormattedText/character #####
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


### As Methods #####
setAs('character', 'FormattedText', function(from)FT_character(from))
setAs('shiny.tag', 'FormattedText', function(from)FT_html(from))
setAs('Rd'       , 'FormattedText', function(from)FT_Rd(from))
setAs('NULL'     , 'FormattedText', function(from)FT())

FormattedText <- FT <- function(from=character(0))as(from, 'FormattedText')
if(FALSE){#@testing FormattedText As Methods
    expect_is(as("hello world", 'FormattedText'), 'FormattedText/character')

        y <- with(htmltools::tags, div('Regular', a('link')))
    expect_is(y, 'shiny.tag')
    expect_is(as(y, 'FormattedText'), 'FormattedText/html')
}

### Class: SectionList #####
#' @exportClass SectionList
setVector( element = "Section"
         , Class   = "SectionList"
         )

### Class: SubSection #####
#' Create a documentation subsection
#'
#' @export
subsection <-
setClass( "SubSection"
        , list( 'FormattedText'
              , title = 'character'
              , content = 'FormattedText'
              )
        )
setValidity("SubSection", function(object){
    validate_that( is(content, 'FormattedText')
                 , length(title) == 1L
                 )
})
setMethod("initialize", "SubSection", function(.Object, title, content){
    assert_that( rlang::is_string(title))
    if (!is(content, 'FormattedText'))
        content <- FT(content)
    .Object@title <- title
    .Object@content <- content
    return(.Object)
})
if(FALSE){#@testing Class: SubSection
    obj <- subsection("Test Subsection"
                     , content = (x <- stringi::stri_rand_lipsum(3))
                     )
    expect_identical(obj@title, "Test Subsection")
    expect_identical(obj@content, FT(x))
}

### Class: Section #####
#' @export
setClass('Section', contains='VIRTUAL')
setValidity('Section', function(object){
    validate_that( all_inherit(object, 'FormattedText')
                 , !any(purrr::map_lgl(object, is, "Section"))
                 )
})
if(FALSE){#@testing Section(Virtual)
    expect_error(new('Section'), "trying to generate an object from a virtual class")
}

asection <- setClass("Section(Anonymous)", contains=c('Section', 'list'))
setValidity("Section(Anonymous)", function(object){
    validate_that( all_inherit(object, 'FormattedText')
                 , !any(purrr::map_lgl(object, is, "Section"))
                 )
})
if(FALSE){#@testing Section(Anonymous)
    bare <- new('Section(Anonymous)')
    expect_is(bare, 'Section(Anonymous)')
    expect_equal(mode(bare), 'list')

    x <- stringi::stri_rand_lipsum(3)
    char <- FT(x)
    html <- FT_html(htmltools::tags$div(purrr::map(x, htmltools::tags$p)))
    rd <- FT_Rd(toRd(char))

    val <- new('Section(Anonymous)', list(char))
    expect_is(val, 'Section')
    expect_is_exactly(val, 'Section(Anonymous)')
    expect_identical(val[[1]], char)

    val <- new('Section(Anonymous)', list(char, html, rd))
    expect_is(val, 'Section(Anonymous)')
    expect_identical(val[[1]], char)
    expect_identical(val[[2]], html)
    expect_identical(val[[3]], rd)
}
section <-
setClass('Section(Titled)', c(title='character' ), contains=c('Section', 'list'))
setValidity('Section(Titled)', function(object){
    validate_that( length(object@title) == 1
                 , !is.na(object@title)
                 , nchar(object@title) > 0
                 )
})
if(FALSE){#@testing Section(Titled)
    bare <- new('Section(Titled)')
    expect_is(bare, 'Section(Titled)')
    expect_equal(mode(bare), 'list')
    expect_error(validObject(bare), "invalid class")

    x <- stringi::stri_rand_lipsum(3)
    char <- FT(x)
    html <- FT_html(htmltools::tags$div(purrr::map(x, htmltools::tags$p)))
    rd <- FT_Rd(toRd(char))

    val <- new('Section(Titled)', list(char), title = "Character Section")
    expect_is(val, 'Section')
    expect_is_exactly(val, 'Section(Titled)')
    expect_identical(val[[1]], char)
    expect_identical(val@title, "Character Section")

    val <- new('Section(Titled)', list(char, html, rd), title = 'Mixed Section')
    expect_is(val, 'Section(Titled)')
    expect_identical(val[[1]], char)
    expect_identical(val[[2]], html)
    expect_identical(val[[3]], rd)
    expect_identical(val@title, "Mixed Section")
}
