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

    x <- FT_Rd(stringi::stri_rand_lipsum(1))
    expect_is(x, 'FormattedText/Rd')
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

# Class: Section(VIRTUAL) =======
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

### Section ≜ FormattedText #####
setIs('Section', 'FormattedText')
if(FALSE){#@testing Section is FormattedText
    x <- asection(list(FT(stringi::stri_rand_lipsum(3))))
    expect_is(x, 'FormattedText')
}


### Class: Section(Anonymous) #####
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

### Class: Section(Titled) #####
section <-
setClass('Section(Titled)', c(title='character' ), contains=c('Section', 'list'))
setValidity('Section(Titled)', function(object){
    validate_that( all_inherit(object, 'FormattedText')
                 , !any(purrr::map_lgl(object, is, "Section"))
                 , length(object@title) == 1
                 , !is.na(object@title)
                 , nchar(object@title) > 0
                 )
})
setMethod('initialize', 'Section(Titled)',
    function(.Object, title, ..., content=list(...)){
        assert_that( missing(content) || ...length() == 0L)
        assert_that( !any(purrr::map_lgl(content, is, "Section"))
                   , is.string(title)
                   )
        .Object@title <- title
        if (!is.list(content))
            content <- list(content)
        for (i in seq_along(content)){
            if (!is(content[[i]], 'FormattedText'))
                content[[i]] <- as(content[[i]], 'FormattedText')
        }
        S3Part(.Object, TRUE) <- content
        return(.Object)
})
if(FALSE){#@testing Section(Titled)
    expect_error(new('Section(Titled)'))
    bare <- new('Section(Titled)', '')

    expect_is(bare, 'Section(Titled)')
    expect_equal(mode(bare), 'list')
    expect_error(validObject(bare), "invalid class")

    x <- stringi::stri_rand_lipsum(3)
    char <- FT(x)
    html <- FT_html(htmltools::tags$div(purrr::map(x, htmltools::tags$p)))
    rd <- FT_Rd(toRd(char))

    val <- new('Section(Titled)', content=list(char), title = "Character Section")
    expect_is(val, 'Section')
    expect_is_exactly(val, 'Section(Titled)')
    expect_identical(val[[1]], char)
    expect_identical(val@title, "Character Section")

    val <- new('Section(Titled)'
              , char, html, rd
              , title = 'Mixed Section')
    expect_is(val, 'Section(Titled)')
    expect_identical(val[[1]], char)
    expect_identical(val[[2]], html)
    expect_identical(val[[3]], rd)
    expect_identical(val@title, "Mixed Section")

    val <- section(title='Testing', content=stringi::stri_rand_lipsum(1))
    expect_is(val, 'Section(Titled)')
}

### Class: SubSection #####
#' Create a documentation subsection
#'
#' @export
subsection <-
setClass( "SubSection"
        , list( 'FormattedText'
              , 'list'
              , title = 'character'
              )
        )
setValidity("SubSection", function(object){
    validate_that( is(S3Part(object, TRUE), 'FormattedText')
                 , !any(purrr::map_lgl(S3Part(object, TRUE), is, 'Section'))
                 , length(title) == 1L
                 )
})
setMethod("initialize", "SubSection",
    function(.Object, title, ...
            , content=list(...)
            ){
        assert_that( missing(content) || ...length() == 0L)
        assert_that( !any(purrr::map_lgl(content, is, "Section"))
                   , is.string(title)
                   )
        .Object@title <- title
        if (!is.list(content))
            content <- list(content)
        for (i in seq_along(content)){
            if (!is(content[[i]], 'FormattedText'))
                content[[i]] <- as(content[[i]], 'FormattedText')
        }
        S3Part(.Object, TRUE) <- content
        return(.Object)
})
if(FALSE){#@testing Class: SubSection
    obj <- subsection("Test Subsection"
                     , content = (x <- stringi::stri_rand_lipsum(3))
                     )
    expect_identical(obj@title, "Test Subsection")
    expect_identical(S3Part(obj, TRUE), list(FT(x)))
}

setMethod('as.list', 'SubSection', function(x){s(S3Part(x, TRUE), title=x@title)})
setMethod('as.list', 'Section(Anonymous)', function(x){S3Part(x, TRUE)})
setMethod('as.list', 'Section(Titled)', function(x){s(S3Part(x, TRUE), title = x@title)})

setAs('SubSection', 'Section', function(from){
    new('Section(Titled)', title=from@title, content=S3Part(from, TRUE))
})
if(FALSE){#@testing
    ss <- subsection('Sub-Section', stringi::stri_rand_lipsum(1))

    sec <- as(ss, 'Section')
    expect_is_exactly(sec, 'Section(Titled)')
    expect_identical(S3Part(sec, TRUE), S3Part(ss, TRUE))
    expect_identical(as.list(sec), as.list(ss))
    expect_identical(attr(as.list(sec), 'title'), 'Sub-Section')
}



### Class: SectionList #####
#' @exportClass SectionList
setVector( element = "Section"
         , Class   = "SectionList"
         )


### c #####
#' @export
c.FormattedText <-
function(...){
    l <- list(...)
    if (...length() == 1) return(..1)
    for (i in seq_along(l)){
        if (!is(l[[i]], 'FormattedText'))
            l[[i]] <- as(l[[i]], 'FormattedText')
    }
    if (all_are_exactly(l, class(..1))) return(as(NextMethod(), class(..1)))
    new('Section(Anonymous)', list(...))
}
#' @export
c.SubSection <- function(...){
    l <- list(...)
    for (i in seq_along(l)){
        if (!is(l[[i]], 'FormattedText'))
            l[[i]] <- as(l[[i]], 'FormattedText')
    }
    new('Section(Anonymous)', l)
}
if(FALSE){#@testing
    part1 <- FT(stringi::stri_rand_lipsum(1))
    part2 <- subsection('Part 2', stringi::stri_rand_lipsum(1))
    part3 <- subsection('Part 3', stringi::stri_rand_lipsum(1))

    expect_identical(c(part1), part1)

    expect_is_exactly(c(part1, 'More Text'), 'FormattedText/character')

    val <- c(part1, part2)
    expect_is(val, 'Section(Anonymous)')
    expect_identical(val[[1]], part1)
    expect_identical(val[[2]], part2)

    expect_identical(as.list(val), list(part1, part2))

    val2 <- c(val, part3)
    expect_is(val2, 'SectionList')
    expect_length(val2, 2L)
    expect_all_inherit(val2, 'Section')
    expect_is(val2[[1]], 'Section(Anonymous)')
    expect_is(val2[[2]], 'Section(Titled)')

    expect_is(c(part2, part3), 'Section')
    expect_is(c(part2, "More Text"), 'Section')
}

