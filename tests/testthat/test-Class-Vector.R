#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Class-Vector.R`')
#line 20 "/rdtf/documentation/R/Class-Vector.R"
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
