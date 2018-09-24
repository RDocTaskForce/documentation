#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Fun-toRd-Documentation.R`')
#line 55 "R/Fun-toRd-Documentation.R"
test_that('toRd,BaseDocumentation-method', {#! @testing
    null.object <- new('BaseDocumentation')

    description <- withr::with_seed(20180921, stringi::stri_rand_lipsum(3))
    description <- Rd_canonize( Rd(collapse(description, '\n\n'))
                              , control=list(wrap.lines = TRUE, wrap.at=72)
                              )
    obj <-
    object <- new( "BaseDocumentation"
                 , author      = c( person('Andrew', 'Redd', email='andrew.redd@hsc.utah.edu')
                                  , person('Drew'  , 'Blue')
                                  )
                 , title       = 'Create function documentation'
                 , description = description
                 , seealso     = Rd_tag('link', Rd_text('documentation-package'))
                 , keywords    = 'internal'
                 , aliases     = 'test-alias'
                 , concepts    = c('test concept', 'testing', 'debugging')
                 , references  = citation()
                 )
    rd <- toRd(object)

    val <- stringi::stri_split_lines1(collapse0(rd))
    expected <- readLines(system.file("expected_output", "toRd-Documentation.Rd", package='documentation'))
    expect_identical(val , expected)

    expect_equal(collapse0(rd[['\\author']]), "\\author{Andrew Redd \\email{andrew.redd@hsc.utah.edu} and Drew Blue}")
    expect_equal(collapse0(rd[['\\title']]), "\\title{Create function documentation}")
    expect_equal(collapse0(rd['\\keyword']), "\\keyword{internal}")
    expect_equal(collapse0(rd['\\alias']), "\\alias{test-alias}")
})
