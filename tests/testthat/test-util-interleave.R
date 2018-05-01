#! This file was automatically produced by documentation::extract_tests on  2018-04-30 17:06:22
#! changes will be overwritten.
context('tests extracted from file `C:/Users/aredd/Box Sync/Projects/rdtf/documentation/R/util-interleave.R`')
#line 15 "C:/Users/aredd/Box Sync/Projects/rdtf/documentation/R/util-interleave.R"
test_that('interleave', {#! @testing
    expect_equal(interleave( 1:3, 4:6 ), c(1,4,2,5,3,6) )
    expect_is(interleave( 1:3, 4:6 ), 'integer' )
    
    expect_error(interleave(1:3, 4))
    
    expect_equal(interleave( 1:3, 4:6, FALSE ), list(1,4,2,5,3,6) )
    expect_equal(interleave( 1:3, letters[1:3] ), c(1,'a',2,'b',3,'c') )
    
})
