#! This file was automatically produced by documentation::extract_tests on  2018-04-30 17:06:19
#! changes will be overwritten.
context('tests extracted from file `C:/Users/aredd/Box Sync/Projects/rdtf/documentation/R/Class-Documentation.R`')
#line 72 "C:/Users/aredd/Box Sync/Projects/rdtf/documentation/R/Class-Documentation.R"
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
#line 94 "C:/Users/aredd/Box Sync/Projects/rdtf/documentation/R/Class-Documentation.R"
test_that('setGeneric("documented", ...)', {#@testing
    object <- function(msg="hello world"){print(msg)}
    dobj <- documented(object, name='object', title="hello world example")
    
    expect_false(is.null(attr(dobj, 'documentation')))
    expect_is(attr(dobj, 'documentation'), 'function-Documentation')
    
})
#line 109 "C:/Users/aredd/Box Sync/Projects/rdtf/documentation/R/Class-Documentation.R"
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
