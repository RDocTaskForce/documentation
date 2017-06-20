#! This file was automatically produced by documentation::extract_tests on  2017-06-20 13:15:39
#! changes will be overwritten.
context('tests extracted from file `/mnt/data/projects/rdtf/documentation/R/util-comma_list.R`')
#line 15 "/mnt/data/projects/rdtf/documentation/R/util-comma_list.R"
test_that('comma_list', {#! @testing
    expect_is(comma_list(1), 'character')
    expect_equal(comma_list(1), '1')
    
    expect_equal(comma_list(1:2), '1 and 2')
    
    expect_equal(comma_list(1:3), '1, 2, and 3')
})
