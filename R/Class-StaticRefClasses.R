#' @include utils.R

getGlobalClassEnvironments <- function(where, .Force=TRUE){
    if (!isNamespace(where) && !identical(where, .GlobalEnv)) where <- asNamespace(where)
    if (exists('.__GlobalClassEnvironments__.', where, inherits = FALSE))
        return(get('.__GlobalClassEnvironments__.', where, inherits = FALSE))
    if (!.Force)
        stop("Cannot find the global statics environment for ", environmentName(where))
    if (environmentIsLocked(where))
        stop("Cannot create global statics environment in ", environmentName(where))
    message("Statics environment not found, creating...")
    globals.env <- new.env(TRUE, parent = where)
    attr(globals.env, 'name') <- environmentName(where) %<<% 'Statics Environment'
    assign('.__GlobalClassEnvironments__.', globals.env, where)
    globals.env
}
if(FALSE){#@testing
    expect_error( getGlobalClassEnvironments('stats', FALSE)
                , "Cannot find the global statics environment for stats" )
    expect_error( getGlobalClassEnvironments('stats', TRUE)
                , "Cannot create global statics environment in stats" )


}
newClassStaticEnv <-
function( Class
        , parent
        , global = getGlobalClassEnvironments(topenv(parent), TRUE)
        ){
    class.static.env <- new.env(TRUE, parent)
    attr(class.static.env, 'name') <- paste(Class, 'Static Environment')
    assign(Class, class.static.env, envir = global)
    assign('static', class.static.env, envir = class.static.env)
    lockBinding('static', class.static.env)
    return(class.static.env)
}

getClassStaticEnv <-
function(classDef, .Force=FALSE, where=.classEnv(classDef, topenv(parent.frame()))){
    if (!is(classDef, 'refClassRepresentation')){
        if (is(classDef, 'envRefClass'))
            classDef <- classDef$.refClassDef
        else
            classDef <- getClass(classDef)
    }
    globals.env <- getGlobalClassEnvironments(where, TRUE)
    Class <- classDef@className
    if (exists(Class, globals.env, inherits = FALSE, mode = 'environment'))
        return(get(Class, globals.env, inherits = FALSE, mode = 'environment'))
    if (!.Force)
        stop(Class, " does not have an associated static environment.")
    newClassStaticEnv(classDef@className, where)
}


setRefClass("STATIC_VARIABLES")
setMethod('initialize', 'STATIC_VARIABLES',
function(.Object, ...){
    Class <- class(.Object)
    classDef <- getClass(Class)
    static.env <- getClassStaticEnv(classDef, TRUE)
    classDef@refMethods$.objectParent <- static.env
    if (!(attr(static.env, 'initialized') %||% FALSE)){
        if (is.function(.init <- classDef@refMethods$static_initialize)){
            environment(.init)<- static.env
            .init()
        }
        attr(static.env, 'initialized') <- TRUE
    }
    .Object <- callNextMethod(.Object, ...)
    parent.env(.Object) <- static.env

    return(.Object)
})
if(FALSE){#@testing
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
}

#' @export
setStaticRefClass <-
function( Class
        , fields = character()
        , static = character()
        , static.const = list()
        , contains = character()
        , methods = list()
        , where= topenv(parent.frame())
        , ...){
    force(where)
    if (length(static.const)){
        static.const <- as.environment(static.const)
        parent.env(static.const) <- where
        attr(static.const, 'name') <- Class %<<% "Static Constant Environment"
        static.const$static.const <- static.const
    } else
        static.const <- where
    static.env <- newClassStaticEnv(Class, static.const)
    attr(static.env, 'initialized') <- FALSE

    if (!identical(static.const, where)){
        static.const$static <- static.env
        lockEnvironment(static.const)
    }
    for (i in seq_along(static)){
        val <- try(new(static[[i]]), silent=TRUE)
        assign(names(static)[[i]],  val, envir=static.env)
    }

    if (!('STATIC_VARIABLES' %in% contains))
        contains <- c('STATIC_VARIABLES', contains)

    generator <- #TODO supress "non-local assignment to non-field names" warnings
        setRefClass( Class
                   , fields = fields
                   , contains = contains
                   , methods = methods
                   , where = where
                   , ...)
    invisible(generator)
}
if(FALSE){#@testing
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
}
if(FALSE){#@testing
    expect_identical( topenv(getClass("RefVector(Export)")@refMethods$.objectParent)
                    , asNamespace('documentation'))
    expect_error( getClassStaticEnv("Documentation", FALSE, asNamespace('documentation'))
                , "Documentation does not have an associated static environment.")
}
if(FALSE){#@testing
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
}
