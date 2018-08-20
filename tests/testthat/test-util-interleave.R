#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `util-interleave.R`')
#line 15 "R/util-interleave.R"
test_that('interleave', {#! @testing
    expect_equal(interleave( 1:3, 4:6 ), c(1,4,2,5,3,6) )
    expect_is(interleave( 1:3, 4:6 ), 'integer' )
    
    expect_error(interleave(1:3, 4))
    
    expect_equal(interleave( 1:3, 4:6, FALSE ), list(1,4,2,5,3,6) )
    expect_equal(interleave( 1:3, letters[1:3] ), c(1,'a',2,'b',3,'c') )
    
})
