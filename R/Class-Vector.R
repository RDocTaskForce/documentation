#' @include utils.R
#' @import extendedRef
NULL

# S4 Vectors -----

#' A Vector with only one type of object
#' @export
setVector <-
function( element                                   #< name of the class that elements of the vector must inherit from.
        , Class   = paste0("Vector(", element, ")") #< Name of the class
        , ...                                       #< passed on, excluding contains, validity
        , contains = character()
        , c.element = TRUE
        , safe.replace = TRUE
        , vector.is.element = isVirtualClass(element)
        , where = topenv(parent.frame())
        ){
    assert_that( is.string(element), is.string(Class)
               , is.flag(c.element), is.flag(safe.replace)
               , isNamespace(where) || identical(where, globalenv())
               )
    val <-
    setClass( Class=Class, contains = c(contains, 'list')
            , validity = function(object){
                    for(i in seq_along(object))
                        if(!inherits(object[[i]], element))
                            return(sprintf("Element of Vector at position %d is not a %s", i, element))
                    return(TRUE)
                }
            , ..., where=where)
    setAs(element, Class, function(from){
        new(Class, list(from))
    }, where=where)

    if (vector.is.element)
        setIs(Class, element, where = where)

    wrapped <- no_src(eval(substitute(function(...){
        as(NextMethod(), Class)
    }, list(Class=Class)), where))
    assign( '[.' %<<<% Class, wrapped, envir = where)
    assign( 'unique.' %<<<% Class, wrapped, envir = where)

    wrapped.c <- no_src(eval(substitute(function(...){
        l <- list(...)
        for (i in seq_along(l)) {
            if (is(l[[i]], Class)) l[[i]] <- S3Part(l[[i]], TRUE) else
            if (is(l[[i]], element)) l[[i]] <- list(l[[i]]) else
            l[[i]] <- list(as(l[[i]], element))
        }
        as(do.call(c, l), Class)
    }, list(Class=Class, element=element))))
    assign( 'c.' %<<<% Class, wrapped.c, envir = where)
    assign( 'c.' %<<<% element, wrapped.c, envir = where) %if% (c.element)

    if (safe.replace) {
        replace <-
        assign('[[<-.' %<<<% Class, envir=where
              , no_src(eval(substitute(function(x, ..., value){
                    if (!is(value, element)) {
                        if (!(canCoerce(value, element)
                            && is(value <- try(as(value, element), silent = TRUE), element)
                            ))
                            stop(._('value does not inherit from class %s,' %<<%
                                    'nor can it be coerced.', sQuote(element)))
                    }
                    S3Part(x, TRUE, 'list')[[...]] <- value
                    invisible(x)
                }, list(Class=Class, element=element)), where))
              )
        assign('[<-.' %<<<% Class, envir = where
              , no_src(eval(substitute(function(x, ..., value){
                    if (!is(value, Class)) {
                        if (!( canCoerce(value, Class)
                            && is(value <- try(as(value, Class), silent = TRUE), Class)
                            ))
                            stop(._('value does not inherit from class %s,' %<<%
                                    'nor can it be coerced.', sQuote(Class)))
                    }
                    S3Part(x, TRUE, 'list')[...] <- S3Part(value, TRUE, 'list')
                    invisible(x)
                }, list(Class=Class, element=element)), where))
              )
    }

    return(val)
}
if(FALSE){#! @testing
    pkgcond::suppress_warnings(pattern="", {
        pkg <- testextra::new_pkg_environment( "test Vector(name) package", register = TRUE
                                             , import=c('methods', 'testthat', 'testextra'))
    })
    pkg$new.class <-
    new.class <- setVector('name', where=pkg)
    expect_is(new.class, "classGeneratorFunction")
    expect_equal(new.class@package, getPackageName(pkg))

    expect_true(exists("[.Vector(name)", pkg))
    expect_true(exists("[<-.Vector(name)", pkg))
    expect_true(exists("[[<-.Vector(name)", pkg))
    expect_true(exists("c.Vector(name)", pkg))
    expect_true(exists("unique.Vector(name)", pkg))
    expect_true(exists("c.name", pkg))


    with(new.env(parent=pkg), {
        expect_is(name.vector <- new('Vector(name)'), 'Vector(name)')
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
    })
    removeVector(new.class@className, where=pkg)
    unloadNamespace(pkg)
}
if(FALSE){#@testing setVector w/ Virtual Class
    pkg <- pkgcond::suppress_warnings(pattern = "replacing previous import", {
        testextra::new_pkg_environment("test Vector w/Virtual package", register=TRUE
                                         , import = c("methods", "testthat", "documentation") )
    })
    tryCatch(with(pkg, {
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
    }), finally = unregister_namespace(pkg))
}


#' @export
removeVector <- function(Class, where=topenv(parent.frame())){
    if (is(Class,"classGeneratorFunction")) Class <- Class@className

    m <- methods(class = Class)
    rm( list = ls( envir=where
                 , pattern="^(\\[|\\[<-|c|unique)\\." %<<<%
                    regex_escape(Class) %<<<% '(<-)?$'
                 )
      ,  envir = where)
    removeClass(Class, where=where)
}

newDocSet <- function( element, ...
                     , methods=list()
                     ){
    elemDef <- getClass(element)
    if ('names' %in% slotNames(elemDef))
        methods$get_names <- function()as.character(unlist(purrr::map(., slot, 'names')))
    if ('package' %in% slotNames(elemDef))
        methods$get_packages <- function()as.character(unlist(purrr::map(., slot, 'package')))
    methods$names <- function()sapply(., format)
    setRefSet( element = element, ...
             , methods=methods
             , static.methods = list(equals = function(x, y)any(doc_get_name(x) %in% doc_get_name(y)))
             )
}
