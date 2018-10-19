#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Class-StaticRefClasses.R`')
#line 17 "/rdtf/documentation/R/Class-StaticRefClasses.R"
test_that('getGlobalClassEnvironments', {#@testing
    expect_error( getGlobalClassEnvironments('stats', FALSE)
                , "Cannot find the global statics environment for stats" )
    expect_error( getGlobalClassEnvironments('stats', TRUE)
                , "Cannot create global statics environment in stats" )


})
#line 75 "/rdtf/documentation/R/Class-StaticRefClasses.R"
test_that('initialize,STATIC_VARIABLES-method', {#@testing
    expect_warning({
        test_static <- setRefClass( "testStaticClass"
                                  , list(count = function().count)
                                  , contains = 'STATIC_VARIABLES'
                                  , methods = list(initialize = function(){
                                        .count <<- .count+1L
                                  }
                                  )
                                  , where = globalenv())
    }, "non-local assignment to non-field names")
    static.env <- getClassStaticEnv("testStaticClass", TRUE, where = globalenv())
    assign('.count', 0L, envir=static.env)

    val <- test_static()

    expect_identical(static.env[['.count']], 1L)
    expect_identical(val$count, 1L)
})
#line 137 "/rdtf/documentation/R/Class-StaticRefClasses.R"
test_that('setStaticRefClass', {#@testing
    expect_warning({
    generator <-
        setStaticRefClass( Class = "testStaticClass"
                         , fields = c( . = 'list'
                                     , count = function().count
                                     )
                         , static = c(.count = 'integer')
                         , static.const = list(element='logical')
                         , methods = list(
                            initialize = function(...){
                                . <<- list(...)
                                assert_that(all_inherit(., element))
                                .count <<- .count+1L
                            },
                            static_initialize = function(){
                                .count <<- 0L
                            },
                            validate = function(){
                                validate_that(all_inherit(., element))
                            },
                            append = function(...){
                                l <- list(...)
                                assert_that(all_inherit(l, element, "`...`"))
                                . <<- c(., ...)
                                invisible(.self)
                            }
                         )
                         , validity = function(object)object$validate()
                         )
    })
    static.env <- getClassStaticEnv("testStaticClass")
    expect_false(attr(static.env, 'initialized'))

    object <- generator(TRUE, FALSE)
    expect_is(object, 'testStaticClass')
    expect_is(object, 'STATIC_VARIABLES')
    expect_is(object, 'envRefClass')
    expect_true(attr(static.env, 'initialized'))

    expect_identical(getClassStaticEnv(object), static.env)

    expect_identical(static.env$.count, 1L)
    expect_identical(object$count, 1L)

    expect_identical(get('element', object), 'logical')

    static.const <- parent.env(static.env)
    expect_equal( environmentName(static.const)
                , "testStaticClass Static Constant Environment")
    expect_equal( environmentName(static.env)
                , "testStaticClass Static Environment")
    expect_error( static.const$msg <- 'Hello world'
                , "cannot add bindings to a locked environment")

    expect_valid(object)

    expect_identical(object$append(FALSE, FALSE, TRUE), object)
    expect_valid(object)

    expect_error(object$append(0L), "bad element at 1")
    expect_length(object$., 5L)

    removeClass("testStaticClass")
})
#line 202 "/rdtf/documentation/R/Class-StaticRefClasses.R"
test_that('setStaticRefClass', {#@testing
    expect_identical( topenv(getClass("RefVector(Export)")@refMethods$.objectParent)
                    , asNamespace('documentation'))
    expect_error( getClassStaticEnv("Documentation", FALSE, asNamespace('documentation'))
                , "Documentation does not have an associated static environment.")
})
#line 208 "/rdtf/documentation/R/Class-StaticRefClasses.R"
test_that('setStaticRefClass', {#@testing
    expect_warning({
    gen <- setStaticRefClass( "no static const"
                            , fields = c( . = 'list'
                                        , count = function().count
                                        )
                            , static = c(.count = 'integer')
                            , methods = list(
                                initialize = function(...).count <<- .count+1L,
                                static_initialize = function().count <<- 0L
                            )
                            , where = globalenv()
                            )
    })

    static <- getClassStaticEnv("no static const", where = globalenv())
    expect_identical(parent.env(static), globalenv())
    removeClass("no static const")
})
