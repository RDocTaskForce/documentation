#! This file was automatically produced by documentation::extract_tests on  2017-06-20 13:15:39
#! changes will be overwritten.
context('tests extracted from file `/mnt/data/projects/rdtf/documentation/R/documentation.R`')
#line 48 "/mnt/data/projects/rdtf/documentation/R/documentation.R"
test_that('documentation<-.,(,,,,,)', {#! @testing
    x <- 1
    y <- new('Documentation', title='testing')
    
    documentation(x) <- y
    expect_identical(attr(x, 'documentation'), y)
    expect_identical(documentation(x), y)
    
    expect_error( documentation(x) <- 'this should not work'
                , 'Documentation can only be set with objects ' %\% 
                  'of, or inheriting from, the `Documentation` class.'
                )
    
})
