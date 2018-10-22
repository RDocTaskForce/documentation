#' @include utils.R
#' @include Class-StaticRefClasses.R

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
               , is.environment(where)
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
}
if(FALSE){#@testing setVector w/ Virtual Class
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

# Reference Vectors -----
### Class: refList #####
refList <- setRefClass( 'refList', fields = list(.='list'),
    methods = list(initialize = function(...){
        . <<- list(...)
    }))
setMethod('[[', c('refList', 'ANY'), function(x, i, ...)(x$.)[[i, ...]])
setMethod('[[<-', c('refList', 'ANY'), function(x, i, ..., value){
    x$.[[i, ...]] <- value
    x
})
setMethod('length', 'refList', function(x)length(x$.))
setMethod('names', 'refList', function(x)names(x$.))
if(FALSE){#@testing
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
}


### Helper Functions #####
.validity_via_ref_method <- function(object){object$validate()}

### Reference Vectors #####
#' @describeIn setVector Create a reference class based vector.
setRefVector <-
function( element
        , Class = paste0("RefVector(", element, ")")
        , fields = list()
        , contains = character()
        , methods = list()
        , ...
        , where = topenv(parent.frame())
        ){
    where <- where
    assert_that( is.string(element), is.string(Class)
               , isNamespace(where) || identical(where, globalenv())
               )
    if (!('initialize' %in% names(methods)))
        methods[["initialize"]] <- function(...){
            add(...)
        }
    if (!('validate' %in% names(methods)))
        methods[['validate']] <- function(){
            validate_that(all_inherit(., element))
        }
    if (!('is_valid' %in% names(methods)))
        methods[['is_valid']] <- function(){
            valid <- validate()
            if (isTRUE(valid)) return(valid) else
            return(s(FALSE, msg=valid))
        }
    if (!('add' %in% names(methods)))
        methods[['add']] <- function(...){
            l <- list(...)
            for (i in rev(seq_along(l))) {
                if (!is(l[[i]], element))
                    try(l[[i]] <- as(l[[i]], element))
            }
            assert_that(all_inherit(l, element, "`...`"))
            . <<- c(., l)
            invisible(.self)
        }
    generator <-
        setStaticRefClass( Class=Class
                         , static.const = list(element=element)
                         , fields = fields
                         , contains = c(contains, 'refList')
                         , methods = methods
                         , validity = .validity_via_ref_method
                         , ..., where=where)
    return(invisible(generator))
}
if(FALSE){#@testing
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
}

### Reference Set #####
#' Reference Sets, Collections of unique objects.
#'
#' A set is defined as a collection of unique objects.
#' The difference from reference vectors in that they
#' do not allow duplicate objects.
setRefSet <-
function( element
        , Class = paste0("RefSet(", element, ")")
        , fields = list()
        , contains = character()
        , methods = list()
        , ...
        , where = topenv(parent.frame())
        , condition.already.contains = .conditions
                #< Type of condition to raise if object is
                #< already contained in the collection.
        , static.const = named()
        , equals = identical
        ){
    where <- where
    condition.already.contains <-
        match.arg(condition.already.contains, .conditions)
    assert_that( is.string(element)
               , is.string(Class)
               , isNamespace(where) || identical(where, globalenv())
               )
    if (!('initialize' %in% names(methods)))
        methods[["initialize"]] <- function(...){
            add(...)
        }
    if (!('validate' %in% names(methods)))
        methods[['validate']] <- function(){
            validate_that( all_inherit(., element)
                         , !anyDuplicated(.)
                         )
        }
    if (!('is_valid' %in% names(methods)))
        methods[['is_valid']] <- function(){
            valid <- validate()
            if (isTRUE(valid)) return(valid) else
            return(s(FALSE, msg=valid))
        }
    if (!('add' %in% names(methods)))
        methods[['add']] <- function(...){
            l <- list(...)
            for (i in rev(seq_along(l))) {
                if (!is(l[[i]], element))
                    try(l[[i]] <- as(l[[i]], element))
                if (any(purrr:::map_lgl(., equals, l[[i]]))) {
                    condition( ._('Set already contains the element given at position %d.', i)
                             , condition.already.contains
                             , type = "already.contains"
                             , scope = c( .refClassDef@package
                                        , .refClassDef@className
                                        , "add")
                             )
                    l <- l[-i]
                }
            }
            assert_that( all_inherit(l, element, "`...`")
                       , !anyDuplicated(l))
            l <- c(., l)
            . <<- l
            if (exists('sort', .refClassDef@refMethods))
                .self$sort()
            invisible(.self)
        }
    assert_that(!any(c("element", "condition.already.contains") %in% names(static.const)))
    static.const <- c(static.const, named(element, condition.already.contains))
    assert_that( is.function(equals), number_of_arguments(equals) >= 2)
    static.const$equals <- equals
    generator <-
        setStaticRefClass( Class=Class
                         , static.const = static.const
                         , fields = fields
                         , contains = c(contains, 'refList')
                         , methods = methods
                         , validity = .validity_via_ref_method
                         , ..., where=where)
    return(invisible(generator))
}
if(FALSE){#@testing
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
}
if(FALSE){#@testing
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
}