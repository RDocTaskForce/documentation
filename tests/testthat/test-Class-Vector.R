#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Class-Vector.R`')
#line 84 "/rdtf/documentation/R/Class-Vector.R"
test_that('setVector', {#! @testing
    new.class <- setVector('name', where=globalenv())
    expect_is(new.class, "classGeneratorFunction")

    expect_is(name.vector <- new.class(), 'Vector(name)')
    name.vector[[1]] <- as.name('a')
    name.vector[[2]] <- as.name('b')
    expect_equal(length(name.vector), 2)
    expect_true(validObject(name.vector))
    expect_error( withr::with_options( list(useFancyQuotes=FALSE)
                                     , name.vector[[3]] <- rnorm
                                     )
                , "value does not inherit from class 'name', nor can it be coerced.")
    expect_error( withr::with_options( list(useFancyQuotes=FALSE)
                                     , name.vector[3] <- 'c'
                                     )
                , "value does not inherit from class 'Vector\\(name\\)', nor can it be coerced\\.")

    name.vector[3] <- as.name('c')

    x <- new.class(list( a <- as.name('a')
                       , b <- as.name('b')
                       ))
    expect_is(x, 'Vector(name)')
    expect_equal(x[[1]], a)
    expect_equal(x[[2]], b)

    expect_is(y <- c(x, b), 'Vector(name)')
    expect_length(y, 3L)

    expect_identical(unique(y), x)

    expect_is(.undefined, 'name')
    expect_identical( as(.undefined, "Vector(name)")
                    , new.class(list(.undefined))
                    )

    expect_false(is(x, 'name'))

    expect_error( new('Vector(name)', list(as.name('a'), as.name('b'), 'c'))
                , "Element of Vector at position 3 is not a name")

    removeVector(new.class@className, where=globalenv())
})
#line 128 "/rdtf/documentation/R/Class-Vector.R"
test_that('setVector w/ Virtual Class', {#@testing setVector w/ Virtual Class
    velement <- setClass('virtual element', where = globalenv())
    expect_true(isVirtualClass(velement@className))

    element <- setClass('logical element', where = globalenv()
                       , contains = c('virtual element', 'logical') )

    vclass <- setVector(velement@className, where =globalenv())

    x <- TRUE
    expect_false(is(x, element@className))
    y <- as(x, element@className)
    expect_is(y, velement@className)
    expect_is_exactly(y, element@className)
    z <- new(vclass@className, list(y))
    expect_is_exactly(z, vclass@className)
    expect_is(z, velement@className)

    removeVector(vclass)
})
