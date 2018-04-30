#! This file was automatically produced by documentation::extract_tests on  2017-06-20 13:15:39
#! changes will be overwritten.
context('tests extracted from file `/mnt/data/projects/rdtf/documentation/R/Vector.R`')
#line 19 "/mnt/data/projects/rdtf/documentation/R/Vector.R"
test_that('setVector', {#! @testing
    new.class <- setVector('name') # creates `Vector()`
    name.vector <- new.class()
    name.vector[[1]] <- as.name('a')
    name.vector[[2]] <- as.name('b')
    expect_equal(length(name.vector), 2)
    expect_true(validObject(name.vector))
    name.vector[[3]] <- 'c'
    expect_error(validObject(name.vector), "Element of Vector at position 3 is not a name")
    
})
