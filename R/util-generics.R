


which.list.recurser <- function(j, l=get('l', parent.frame())){
    x= which.list(l[[j]])
    if(is.null(x)) return(j)
    lapply( x, function(x)c(j, x))
}
which.list <- function(l){
    if(is.list(l)){
        i <- which(sapply(l, function(.)any(unlist(.))))
        sapply(i, which.list.recurser, l=l)
    }
}
if(FALSE){#@testing
    l <- list( list(F, T)
               , F
               , list(F, list(F, F, list(F, T, F)))
    )
    expect_equal(which.list(l), list(c(1,2), c(3,2,3,2)))
}

.is_UseMethod <- function(expr){
    if (!is.call(expr)) return(FALSE)
    if ( 2 <= length(expr) && length(expr) <= 3
       && is.name(expr[[1]]) && expr[[1]] == 'UseMethod'
       ) return(TRUE)
    lapply(expr, .is_UseMethod)
}
.find_UseMethod <- function(expr){
    which.list(.is_UseMethod(expr))
}
isS3Generic <- function(f){
    assert_that( is.function(f))
    any(all.names(body(f)) =="UseMethod")
}
whichS3Generic <- function(f){
    if (!isS3Generic(f)) return(NULL)
    sapply( .find_UseMethod(body(f))
          , function(loc)body(f)[[loc]][[2L]]
          )
}
if(FALSE){#@testing
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

    f <- function(x){
        assert_that(is.atomic(x))
        UseMethod("test_generic", html)
    }
    expect_true(isS3Generic(f))
    expect_true(any(unlist(.is_UseMethod(body(f)))))

    expect_null(whichS3Generic(rnorm))
}

is_S3_method_call <- function(which=-1){
    if (which < 0) which <- sys.nframe() + which
    if(abs(which) >= sys.nframe()) return(FALSE)           # nocov
    if(is.null(fun<- sys.function(which-1))) return(FALSE) # nocov
    isS3Generic(fun)
}
if(FALSE){#@testing
    print.my_class <- function(x, ...){return(invisible(is_S3_method_call()))}

    val <- print(s(list(), class="my_class"))
    expect_true(val)

    val <- print.my_class(s(list(), class="my_class"))
    expect_false(val)
    expect_false(is_S3_method_call())
}
get_S3_method_specialization <-
function(which=-1){
    if (which < 0) which <- sys.nframe() + which
    if (is_S3_method_call(which)){
        parent <- sys.call(which-1L)
        call <- sys.call(which)

        gsub( "^"%<<<%parent[[1]]%<<<%"\\.", ''
              , deparse(call[[1]])
        )
    }
}
if(FALSE){#@testing
    print.my_class <- function(x, ...)return(invisible(get_S3_method_specialization()))

    val <- print(s(list(), class="my_class"), which-1)
    expect_equal(val, 'my_class')

    val <- print.my_class(s(list(), class="my_class"))
    expect_null(val)
}

is_S4_method_call <- function(which=-1L){
    if (which < 0) which <- sys.nframe() + which
    if (which >= sys.nframe()) return(FALSE)             # nocov
    inherits(sys.function(which), "MethodDefinition") ||
        (  sys.call(which)[[1L]] == '.local'
           && sys.call(which-1L) == sys.call(which-2L)
        )
}
if(FALSE){#@testing
    try(removeClass('test_class', where=globalenv()), silent=TRUE)
    setClass('test_class', contains='list', where=globalenv())
    setMethod('show', 'test_class', function(object){
        invisible(is_S4_method_call())
    }, where=globalenv())
    other_show <- function(object){
        is_S4_method_call()
    }
    object <- new('test_class')

    val <- show(object)
    expect_true(val)
    expect_false(other_show(object))
    expect_false(is_S4_method_call())
}
get_S4_method_specialization <-
    function(which=-1){
        if (inherits(fun <- sys.function(which), "MethodDefinition"))
            return(fun@target[[1]])
        if(sys.call(which)[[1L]] == '.local'
           && sys.call(which-1L) == sys.call(which-2L)) {
            # name <- deparse(sys.call(which-1L)[[2]])
            frame <- sys.frame(which-1L)
            return(as.character(get('.target', envir=frame)))
        } else
            stop("Could not determine target of S4 method.") # nocov
    }
if(FALSE){#@testing
    setClass('test_class','list', where=globalenv())
    setMethod('show', 'test_class', function(object){
        invisible(get_S4_method_specialization())
    }, where=globalenv())
    setGeneric('get_spec', function(object)get_S4_method_specialization(), where=globalenv())
    object <- new('test_class')

    val <- show(object)
    expect_equal(val, 'test_class')

    val <- get_spec(object)
    expect_equal(as.character(val), 'test_class')
    expect_true(removeClass('test_class', where = globalenv()))
}

is_registered_S3method <- function(fun, env = topenv(environment(fun))){
    if (!isNamespace(env))
        doc_warning("S3 methods are only registered inside namespace environments.")
    if (exists('.__S3MethodsTable__.', envir = env, inherits = FALSE)){
        for (fname in ls(env$.__S3MethodsTable__.))
            if (identical(fun, get(fname, envir=env$.__S3MethodsTable__.)))
                return(TRUE)
    } else if (getPackageName(env) == 'base'){
        base.s3 <- apply(base::.S3_methods_table, 1, collapse, with='.')
        name <- name_me(fun, envir=env)
        return(name %in% base.s3)
    }
    return(FALSE)
}
if(FALSE){#@testing
    fun <- getAnywhere("get_parse_data.srcfile")[1]
    expect_true(is_registered_S3method(fun))
    expect_true(is_registered_S3method(unique.array))
    expect_false(is_registered_S3method(toRd))
    expect_warning(is_registered_S3method(toRd, environment(toRd)))
}

name_me <- function(obj, envir=environment(obj)){
    assert_that(is.environment(envir))
    objects <- ls(envir)
    if (exists('.__NAMESPACE__.', envir = envir) &&
        exists('lazydata', envir = envir$.__NAMESPACE__.))
        objects <- c(objects, ls(envir$.__NAMESPACE__.$lazydata))
    for (name in objects)
        if (identical(obj, get(name, envir=envir)))
            return(name)
    return(NULL)
}
if(FALSE){#@testing
    obj <- unique.array
    expect_identical(name_me(obj), "unique.array")


    obj <- iris
    expect_identical(name_me(iris, asNamespace('datasets')), 'iris')

    expect_error(name_me(iris))
    expect_null(name_me(iris, rlang::base_env()))
}


funky <- function(expr, vars=parent.frame(), fun.env=parent.frame()){
    expr <- rlang::enexpr(expr)
    no_src(eval(expr, fun.env))
}
if(FALSE){#@testing
    element <- 'Element'
    expected <- function()"Element"
    val <- funky(function()!!element)
    expect_identical(val, expected)
}
