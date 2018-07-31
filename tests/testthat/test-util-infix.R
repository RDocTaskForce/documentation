#! This file was automatically produced by documentation::extract_tests on  2018-05-23 19:03:27
#! changes will be overwritten.
context('tests extracted from file `util-infix.R`')
#line 18 "/rdtf/documentation/R/util-infix.R"
test_that('%<<%', {#! @testing %<<%
    a <- 'A vain chalks above the integrated biscuit. '
    b <- 'Within the ground burns the leader.'
    c <- 'How can the fifteen distress lose?'
    expect_equal(a %<<% b, paste(a,b))
    expect_equal(a %<<% b %<<% c, paste(a,b,c))
})
#line 28 "/rdtf/documentation/R/util-infix.R"
test_that('%<<<%', {#! @testing %<<<%
    a <- 'A vain chalks above the integrated biscuit. '
    b <- ' Within the ground burns the leader.'
    c <- 'How can the fifteen distress lose?'
    expect_equal(a %\% b, paste(a,b, sep='\n'))
    expect_equal(a %\% b %\% c, paste(a,b, c, sep='\n'))
})
#line 37 "/rdtf/documentation/R/util-infix.R"
test_that('newline-concatenation', {#! @testing newline-concatenation
    a <- 'A vain chalks above the integrated biscuit. '
    b <- ' Within the ground burns the leader.'
    c <- 'How can the fifteen distress lose?'
    expect_equal(a %\% b, paste(a,b, sep='\n'))
    expect_equal(a %\% b %\% c, paste(a,b, c, sep='\n'))
})
