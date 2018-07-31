#! This file was automatically produced by documentation::extract_tests on  2018-05-23 19:03:27
#! changes will be overwritten.
context('tests extracted from file `documentation.R`')
#line 48 "/rdtf/documentation/R/documentation.R"
test_that('documentation<-,ANY,Documentation-method', {#! @testing
    x <- 1
    y <- new('Documentation', title='testing')
    
    documentation(x) <- y
    expect_identical(attr(x, 'documentation'), y)
    expect_identical(documentation(x), y)
    
    expect_error( documentation(x) <- 'this should not work'
                , 'Documentation can only be set with objects ' %<<% 
                  'of, or inheriting from, the `Documentation` class.'
                )
    
})
