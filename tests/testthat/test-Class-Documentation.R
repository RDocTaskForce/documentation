#! This file was automatically produced by documentation::extract_tests on  2017-06-20 13:15:39
#! changes will be overwritten.
context('tests extracted from file `/mnt/data/projects/rdtf/documentation/R/Class-Documentation.R`')
#line 72 "/mnt/data/projects/rdtf/documentation/R/Class-Documentation.R"
test_that('initialize.Documentation', {#!@testing
    x <- new('Documentation')
    expect_identical(x@author, person())
    expect_identical(x@title, character(0))
    
    x <- new('Documentation'
            , author     = person('Andrew', 'Redd')
            , references = citation()
            )
    expect_equal(x@author, person('Andrew', 'Redd'))
})
#line 104 "/mnt/data/projects/rdtf/documentation/R/Class-Documentation.R"
test_that('as.list.Documentation', {#! @testing
    x <- 
    object <- new( "Documentation"
             , author      = c( person('Andrew', 'Redd', email='andrew.redd@hsc.utah.edu')
                              , person('Drew'  , 'Blue')
                              )
             , title       = 'Create function documentation'
             , description = stringi::stri_rand_lipsum(3)
             , seealso     = '\\link{documentation-package}'
             , keywords    = 'internal'
             , aliases     = 'test-alias'
             , references  = citation()
             )
    object.as.list <- as.list(object)
    expect_is(object.as.list, 'list')
    expect_equal(names(object.as.list), slotNames(object))
    
    
})
