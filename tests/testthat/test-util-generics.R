#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `util-generics.R`')
#line 15 "R/util-generics.R"
test_that('which.list', {#@testing
    l <- list( list(F, T)
               , F
               , list(F, list(F, F, list(F, T, F)))
    )
    expect_equal(which.list(l), list(c(1,2), c(3,2,3,2)))
})
#line 43 "R/util-generics.R"
test_that('whichS3Generic', {#@testing
    f <- gen <- function(x, l = TRUE, ...){
        if (l){
            y <- UseMethod("gen")
            stopifnot(y>0)
        } else
            stop("in the name of love")
    }
    expect_identical(.find_UseMethod(body(f)), list(c(2L, 3L, 2L, 3L)))

    expect_identical(whichS3Generic(f), "gen")

    g <- function(x, case){
        switch( case
              , a = UseMethod("case_a")
              , b = UseMethod("case_b")
              , c = UseMethod("case_c")
              )
    }
    expect_identical(whichS3Generic(g), c('case_a', 'case_b', 'case_c') )

    f <- html_to_Rd
    expect_true(isS3Generic(f))
    expect_true(any(unlist(.is_UseMethod(body(f)))))
    # expect_identical(.find_UseMethod(body(f)), 2L)

    expect_null(whichS3Generic(rnorm))
})
#line 78 "R/util-generics.R"
test_that('is_S3_method_call', {#@testing
    print.my_class <- function(x, ...){return(invisible(is_S3_method_call()))}

    val <- print(s(list(), class="my_class"))
    expect_true(val)

    val <- print.my_class(s(list(), class="my_class"))
    expect_false(val)
    expect_false(is_S3_method_call())
})
#line 100 "R/util-generics.R"
test_that('get_S3_method_specialization', {#@testing
    print.my_class <- function(x, ...)return(invisible(get_S3_method_specialization()))

    val <- print(s(list(), class="my_class"), which-1)
    expect_equal(val, 'my_class')

    val <- print.my_class(s(list(), class="my_class"))
    expect_null(val)
})
#line 118 "R/util-generics.R"
test_that('is_S4_method_call', {#@testing
    setClass('test_class','list')
    setMethod('show', 'test_class', function(object){
        invisible(is_S4_method_call())
    })
    other_show <- function(object){
        is_S4_method_call()
    }
    object <- new('test_class')

    val <- show(object)
    expect_true(val)
    expect_false(other_show(object))
    expect_false(is_S4_method_call())
})
#line 147 "R/util-generics.R"
test_that('get_S4_method_specialization', {#@testing
    setClass('test_class','list')
    setMethod('show', 'test_class', function(object){
        invisible(get_S4_method_specialization())
    })
    setMethod('toRd', 'test_class', function(obj){
        get_S4_method_specialization()
    })
    other_show <- function(object){
        is_S4_method_call()
    }
    object <- new('test_class')

    val <- show(object)
    expect_equal(val, 'test_class')

    val <- toRd(object)
    expect_equal(as.character(val), 'test_class')
})
