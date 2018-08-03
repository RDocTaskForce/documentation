#! This file was automatically produced by documentation::extract_tests on  2018-08-03 01:24:28
#! changes will be overwritten.
context('tests extracted from file `Fun-toRd-Documentation.R`')
#line 23 "/rdtf/documentation/R/Fun-toRd-Documentation.R"
test_that('toRd,Documentation-method', {#! @testing
    null.object <- new('Documentation')
    
    object <- new( "Documentation"
                 , author      = c( person('Andrew', 'Redd', email='andrew.redd@hsc.utah.edu')
                                  , person('Drew'  , 'Blue')
                                  )
                 , title       = 'Create function documentation'
                 , description = stringi::stri_rand_lipsum(3)
                 , seealso     = '\\link{documentation-package}'
                 , keywords    = 'internal'
                 , aliases     = 'test-alias'
                 , concepts    = 'test concept'
                 , references  = citation()
    )
    as.rd <- toRd(object)
    expected.names <- c( 'author', 'title', 'description', 'seealso', 'keywords'
                         , 'aliases', 'references', 'concepts'
    )
    expect_true(all(names(as.rd) %in% expected.names))
    expect_true(all(expected.names %in% names(as.rd)))
    
    expect_equal(as.rd[['author']], "\\author{Andrew Redd \\email{andrew.redd@hsc.utah.edu} and Drew Blue}")
    expect_equal(as.rd[['title']], "\\title{Create function documentation}")
    expect_equal(as.rd[['keywords']], "\\keyword{internal}")
    expect_equal(as.rd[['aliases']], "\\alias{test-alias}")
})
