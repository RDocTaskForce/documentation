#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Class-FormattedText.R`')
#line 11 "R/Class-FormattedText.R"
test_that('FormattedText/html', {#@testing FormattedText/html
    html <- with(htmltools::tags, p("html formatted text"))
    expect_is(html, 'shiny.tag')
    obj <- FT_html(html)
    expect_is(obj, 'shiny.tag')
    expect_is(obj, 'FormattedText/html')
    expect_is(obj, 'FormattedText')
})
#line 36 "R/Class-FormattedText.R"
test_that('FormattedText/Rd', {#@testing FormattedText/Rd
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
})
#line 84 "R/Class-FormattedText.R"
test_that('FormattedText/character', {#@testing FormattedText/character
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
})
#line 107 "R/Class-FormattedText.R"
test_that('FormattedText As Methods', {#@testing FormattedText As Methods
    expect_is(as("hello world", 'FormattedText'), 'FormattedText/character')

        y <- with(htmltools::tags, div('Regular', a('link')))
    expect_is(y, 'shiny.tag')
    expect_is(as(y, 'FormattedText'), 'FormattedText/html')
})
#line 139 "R/Class-FormattedText.R"
test_that('Class: SubSection', {#@testing Class: SubSection
    obj <- subsection("Test Subsection"
                     , content = (x <- stringi::stri_rand_lipsum(3))
                     )
    expect_identical(obj@title, "Test Subsection")
    expect_identical(obj@content, FT(x))
})
#line 155 "R/Class-FormattedText.R"
test_that('Section(Virtual)', {#@testing Section(Virtual)
    expect_error(new('Section'), "trying to generate an object from a virtual class")
})
#line 165 "R/Class-FormattedText.R"
test_that('Section(Anonymous)', {#@testing Section(Anonymous)
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
})
#line 194 "R/Class-FormattedText.R"
test_that('Section(Titled)', {#@testing Section(Titled)
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
})
