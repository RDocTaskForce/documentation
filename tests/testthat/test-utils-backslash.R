#! This file was automatically produced by documentation::extract_tests on  2017-06-20 13:15:39
#! changes will be overwritten.
context('tests extracted from file `/mnt/data/projects/rdtf/documentation/R/utils-backslash.R`')
#line 4 "/mnt/data/projects/rdtf/documentation/R/utils-backslash.R"
test_that('backslash', {#! @testing backslash
    a <- 'A vain chalks above the integrated biscuit. '
    b <- 'Within the ground burns the leader.'
    c <- 'How can the fifteen distress lose?'
    expect_equal(a %\% b, paste(a,b))
    expect_equal(a %\% b %\% c, paste(a,b,c))
})
#line 14 "/mnt/data/projects/rdtf/documentation/R/utils-backslash.R"
test_that('newline-concatenation', {#! @testing newline-concatenation
    a <- 'A vain chalks above the integrated biscuit. '
    b <- ' Within the ground burns the leader.'
    c <- 'How can the fifteen distress lose?'
    expect_equal(a %\n% b, paste(a,b, sep='\n'))
    expect_equal(a %\n% b %\n% c, paste(a,b, c, sep='\n'))
})
