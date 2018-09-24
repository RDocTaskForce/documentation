#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Class-FormattedText.R`')
#line 10 "R/Class-FormattedText.R"
test_that('FormattedText/html', {#@testing FormattedText/html
    html <- with(htmltools::tags, p("html formatted text"))
    expect_is(html, 'shiny.tag')
    obj <- FT_html(html)
    expect_is(obj, 'shiny.tag')
    expect_is(obj, 'FormattedText/html')
    expect_is(obj, 'FormattedText')
})
#line 33 "R/Class-FormattedText.R"
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
#line 80 "R/Class-FormattedText.R"
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
