#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Class-Vector.R`')
#line 88 "/rdtf/documentation/R/Class-Vector.R"
test_that('setVector', {#! @testing
    new.class <- setVector('name', where=globalenv())
    expect_is(new.class, "classGeneratorFunction")

    expect_is(name.vector <- new.class(), 'Vector(name)')
    name.vector[[1]] <- 'a'
    name.vector[[2]] <- 'b'
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

    expect_error(name.vector[3] <- 'c')
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


    y <- new.class(list( as.name('a')))
    z <- c(y, 'b')
    expect_is(z, "Vector(name)")
    expect_length(z, 2L)

    removeVector(new.class@className, where=globalenv())
})
#line 139 "/rdtf/documentation/R/Class-Vector.R"
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

    expect_true(removeVector(vclass, where = globalenv()))
})
#line 187 "/rdtf/documentation/R/Class-Vector.R"
test_that('names,refList-method', {#@testing
    val <- refList('a', 1L, TRUE)
    expect_is(val, 'refList')
    expect_is(val, 'envRefClass')

    expect_identical(val[[1]], 'a')
    expect_identical(val[[2]], 1L)
    expect_identical(val[[3]], TRUE)

    y <- val
    y[[4]] <- 'testing'
    expect_identical(val[[4]], 'testing')

    expect_length(val, 4L)
    expect_null(names(val))
})
#line 258 "/rdtf/documentation/R/Class-Vector.R"
test_that('setRefVector', {#@testing
    test_vector <- setRefVector('logical')
    expect_is(test_vector, 'refObjectGenerator')

    bare <- test_vector()
    expect_is(bare, "RefVector(logical)")
    expect_length(bare, 0L)
    expect_identical(get('element', bare), 'logical')

    val <- test_vector(a=TRUE, b=FALSE)
    expect_is(val, "RefVector(logical)")
    expect_identical(get('element', val), 'logical')
    expect_length(val, 2L)
    expect_identical(val[[1]], TRUE)
    expect_identical(val[['a']], TRUE)
    expect_identical(val[[2]], FALSE)
    expect_identical(val[['b']], FALSE)

    new <- c(c=NA)
    val$add(c=NA)
    expect_length(val, 3L)
    expect_identical(val[[3]], NA)

    expect_true(val$is_valid())

    val$add(1)
    expect_length(val, 4L)
    expect_true(val$is_valid())

    val$. <- as.list(letters[1:5])
    expect_false(val$is_valid())
})
#line 373 "/rdtf/documentation/R/Class-Vector.R"
test_that('setRefSet', {#@testing
    test_set <- setRefSet('Documentation', where =globalenv()
                         , equals = function(x, y)doc_get_name(x) == doc_get_name(y)
                         , methods = list(sort = function(){
                             if (length(.)>1L){
                                o <- order(sapply(., doc_get_name))
                                . <<- .[o]
                             }
                             invisible(.self)
                         })
                         )

    expect_is(test_set, 'refObjectGenerator')

    my.set <- test_set()
    expect_is(my.set, 'RefSet(Documentation)')
    expect_length(my.set, 0L)

    my.set$add(function_documentation('test_fun'))
    expect_length(my.set, 1L)

    expect_message( my.set$add(function_documentation('test_fun', title = "testing function"))
                  , class = "RefSet(Documentation)-message-already.contains"
                  )
    my.set$add(function_documentation('another'))
    expect_length(my.set, 2L)
    expect_identical( sapply(my.set$., doc_get_name)
                    , c('another', 'test_fun'))
})
#line 402 "/rdtf/documentation/R/Class-Vector.R"
test_that('setRefSet', {#@testing
    test_export_set <- setRefSet( 'Export'
                                , equals = function(e1, e2)format(e1)==format(e2)
                                )
    a <- export('a')
    b <- export('b')

    test.set <- test_export_set(a,b)
    test.set$add('c')

    expect_true(test.set$is_valid())

    test.set$. <- list(a, b, 'c')

    expect_false(test.set$is_valid())
})
