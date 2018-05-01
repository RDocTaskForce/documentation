#! This file was automatically produced by documentation::extract_tests on  2018-04-30 17:06:21
#! changes will be overwritten.
context('tests extracted from file `C:/Users/aredd/Box Sync/Projects/rdtf/documentation/R/util-comma_list.R`')
#line 15 "C:/Users/aredd/Box Sync/Projects/rdtf/documentation/R/util-comma_list.R"
test_that('comma_list', {#! @testing
    expect_is(comma_list(1), 'character')
    expect_equal(comma_list(1), '1')
    
    expect_equal(comma_list(1:2), '1 and 2')
    
    expect_equal(comma_list(1:3), '1, 2, and 3')
})
