#' @include setup.R
#' @include utils.R
#' @include Class-Vector.R

# Class: NamespaceEntry(Virtual) -----
#' Namespace Entries
#'
#' These Classes encapsulate the information to create the NAMESPACE file
#' dynamically.
#'
#' @export
setClass('NamespaceEntry', contains = 'VIRTUAL')

## Class: ExportEntry(VIRTUAL) =====
#' @export
setClass('ExportEntry', contains = c('VIRTUAL', 'NamespaceEntry'))

### Class: Export #####
#' @export
export <-
setClass('Export', contains = 'ExportEntry'
        , list(name='character'))
setMethod('initialize', 'Export', function(.Object, name){
    assert_that( is.string(name)
               , !is.na(name)
               , nchar(name) > 0L
               )
    .Object@name <- name
    return(.Object)
})
#' @export
format.Export <- function(x, ...){
    paste0( 'export(', namespace_quote(x@name), ')')
}
if(FALSE){#@testing
    e <- export('my_name')
    expect_is(e, 'Export')
    expect_identical(e@name, 'my_name')

    expect_error(export())
    expect_error(export(NA_character_))
    expect_error(export(1))
    expect_error(export(TRUE))
    expect_error(export(letters))

    expect_identical( format(export("%<<%"))
                    , 'export("%<<%")')
}

### Class: ExportPattern #####
#' @export
export_pattern <-
setClass('ExportPattern', contains='ExportEntry', list(pattern='character'))
setMethod('initialize', 'ExportPattern', function(.Object, pattern){
    .Object@pattern <- pattern
    assert_that(validObject(.Object))
    return(.Object)
})
setValidity('ExportPattern', function(object){
    validate_that( is.string(object@pattern)
                 , !is.na(object@pattern)
                 , is_valid_regex(object@pattern)
                 )
})
setAs('character', 'ExportPattern', function(from)new('ExportPattern', from))
#' @export
format.ExportPattern <- function(x, ...){
    paste0('exportPattern(\"', x@pattern, '\")')
}
if(FALSE){#@testing
    val <- export_pattern("^[^\\.]")
    expect_is(val, 'NamespaceEntry')
    expect_is(val, 'ExportEntry')
    expect_is(val, 'ExportPattern')
    expect_valid(val)

    expect_error(export_pattern("^[^\\."))

    val2 <- as("^[^\\.]", "ExportPattern")
    expect_identical(val, val2)

    expect_identical(format(val), 'exportPattern("^[^\\.]")')
}

### Class: ExportS3method #####
#' @export
export_s3method <-
setClass( 'ExportS3method', contains = 'ExportEntry'
        , list( generic = 'character'
              , signature = 'character'
              , method = 'character'))
setValidity('ExportS3method', function(object){
    validate_that( is_nonempty_string(object@generic)
                 , is_nonempty_string(object@signature)
                 , is_optional_string(object@method)
                 )
})
setMethod('initialize', 'ExportS3method',
function(.Object, generic, signature, method=character(0)){
    .Object@generic <- generic
    .Object@signature <- signature
    .Object@method <- method
    validObject(.Object)
    return(.Object)
})
#' @export
format.ExportS3method <- function(x, ...){
    paste0( 'S3method(', namespace_quote(x@generic)
                  , ",", namespace_quote(x@signature)
                  , paste0(","
                  , namespace_quote(x@method)
                  ) %if% (length(x@method) > 0L)
                  , ')')
}
if(FALSE){#@testing Class: ExportS3method
    expect_error(export_s3method())
    expect_error(export_s3method('c'))
    val <- export_s3method('c', 'Rd')
    expect_is(val, 'ExportS3method')
    expect_equal(val@generic, 'c')
    expect_equal(val@signature, 'Rd')
    expect_equal(val@method, character(0))

    val <- export_s3method('print', 'class', 'my_special_printer')
    expect_equal(val@generic, 'print')
    expect_equal(val@signature, 'class')
    expect_equal(val@method, 'my_special_printer')

    expect_identical( format(export_s3method('print', 'class'))
                    , "S3method(print,class)" )
    expect_identical( format(export_s3method('print', 'class', "my print function"))
                    , "S3method(print,class,\"my print function\")" )
    expect_identical( format(export_s3method('[', 'my class'))
                    , 'S3method("[","my class")' )
}

### Class: ExportS4methods #####
#' @export
export_s4methods <-
setClass( 'ExportS4methods', contains = 'ExportEntry'
        , list(generic='character'))
setValidity('ExportS4methods', function(object){
    validate_that(is_nonempty_string(object@generic))
})
setMethod("initialize", "ExportS4methods", function(.Object, generic){
    .Object@generic <- generic
    validObject(.Object)
    return(.Object)
})
#' @export
format.ExportS4methods <- function(x, ...){
    paste0( 'exportMethods(', namespace_quote(x@generic), ')')
}
if(FALSE){#@testing Class: ExportS4methods
    expect_error(export_s4methods())
    expect_error(export_s4methods(''))

    val <- export_s4methods('doc_get_name')
    expect_is(val, 'ExportS4methods')
    expect_equal(val@generic, 'doc_get_name')
    expect_valid(val)

    val <- export_s4methods('[[')
    expect_identical(format(val), 'exportMethods("[[")')
}

### Class: ExportS4class #####
#' @export
export_class <-
setClass('ExportS4class', contains = 'ExportEntry'
        , list(name='character'))
setValidity('ExportS4class', function(object){
    validate_that(is_nonempty_string(object@name))
})
setMethod('initialize', 'ExportS4class', function(.Object, class){
    .Object@name <- class
    validObject(.Object)
    return(.Object)
})
#' @export
format.ExportS4class <- function(x, ...){
    paste0( 'exportClasses(', namespace_quote(x@name), ')')
}
if(FALSE){#@testing Class: ExportS4class
    expect_error(export_class())
    expect_error(export_class(''))
    expect_error(export_class(NA))

    val <- export_class('Documentation')
    expect_is(val, 'ExportS4class')
    expect_equal(val@name, 'Documentation')

    expect_identical(format(val), "exportClasses(Documentation)")
}

### Class: ImportEntry(VIRTUAL) =====
#' @export
setClass( 'ImportEntry', contains = c('NamespaceEntry', 'VIRTUAL'))

### Class: Import #####
#' @export
import <-
setClass('Import', contains = c('ImportEntry')
        , list(package = 'character') )
setValidity('Import', function(object){
    validate_that(is_nonempty_string(object@package))
})
setMethod('initialize', 'Import', function(.Object, package){
    .Object@package <- package
    validObject(.Object)
    return(.Object)
})
#' @export
format.Import <- function(x, ...){
    paste0( 'import(', namespace_quote(x@package), ')')
}
if(FALSE){#@testing Class: Import
    expect_error(import())
    expect_error(import(''))

    val <- import('parsetools')
    expect_is(val, 'Import')
    expect_identical(val@package, 'parsetools')

    expect_identical(format(val), 'import(parsetools)')
}

### Class: ImportFrom #####
#' @export
import_from <-
setClass( 'ImportFrom', contains='ImportEntry'
        , list(package='character', names='character'))
setValidity('ImportFrom', function(object){
    validate_that( is_nonempty_string(object@package)
                 , length(object@names) > 0L
                 , !any(is.na(object@names))
                 , !any(nchar(object@names) == 0L)
                 )
})
setMethod('initialize', 'ImportFrom', function(.Object, package, names){
    .Object@package <- package
    .Object@names <- names
    validObject(.Object)
    return(.Object)
})
#' @export
format.ImportFrom <- function(x, ...){
    paste0( 'importFrom('
          , namespace_quote(x@package), ','
          , namespace_quote(x@names), ')')
}
if(FALSE){#@testing Class: ImportFrom
    expect_error(import_from())
    expect_error(import_from('pkg'))
    expect_error(import_from('pkg', ''))
    expect_error(import_from('pkg', c('a', NA)))

    val <- import_from('pkg', 'name')
    expect_is(val, 'ImportFrom')
    expect_equal(val@package, 'pkg')
    expect_equal(val@names, 'name')

    val <- import_from('pkg', c('a', 'b'))
    expect_is(val, 'ImportFrom')
    expect_equal(val@package, 'pkg')
    expect_equal(val@names, c('a', 'b'))

    expect_identical( format(val)
                    , c("importFrom(pkg,a)", "importFrom(pkg,b)"))
}

### Class: ImportClassesFrom #####
#' @export
import_classes_from <-
setClass( 'ImportClassesFrom', contains='ImportEntry'
        , list(package='character', names='character'))
setValidity('ImportClassesFrom', function(object){
    validate_that( is_nonempty_string(object@package)
                 , length(object@names) > 0L
                 , !any(is.na(object@names))
                 , !any(nchar(object@names) == 0L)
                 )
    #TODO add checking that the classes are exported from package
})
setMethod('initialize', 'ImportClassesFrom', function(.Object, package, names){
    .Object@package <- package
    .Object@names <- names
    validObject(.Object)
    return(.Object)
})
#' @export
format.ImportClassesFrom <-function(x, ...){
    paste0( "importClassesFrom("
          , namespace_quote(x@package), ','
          , namespace_quote(x@names)
          , ")")
}
if(FALSE){#@testing Class: ImportClassesFrom
    expect_error(import_classes_from())
    expect_error(import_classes_from('pkg'))
    expect_error(import_classes_from('pkg', ''))

    val <- import_classes_from('pkg', 'my_class')
    expect_is(val, 'ImportClassesFrom')
    expect_equal(val@package, 'pkg')
    expect_equal(val@names, 'my_class')

    val <- import_classes_from('pkg', c('class1', 'class2'))
    expect_is(val, 'ImportClassesFrom')
    expect_equal(val@package, 'pkg')
    expect_equal(val@names, c('class1', 'class2'))

    val <- import_classes_from('pkg', c('class1', 'class 2'))
    expect_identical( format(val)
                    , c( 'importClassesFrom(pkg,class1)'
                       , 'importClassesFrom(pkg,"class 2")'
                       ))
}

### Class: ImportMethodsFrom #####
#' @export
import_methods_from <-
setClass( 'ImportMethodsFrom', contains='ImportEntry'
        , list(package='character', names='character'))
setValidity('ImportMethodsFrom', function(object){
    validate_that( is_nonempty_string(object@package)
                 , length(object@names) > 0L
                 , !any(is.na(object@names))
                 , !any(nchar(object@names) == 0L)
                 )
    #TODO add checking that the classes are exported from package
})
setMethod('initialize', 'ImportMethodsFrom', function(.Object, package, names){
    .Object@package <- package
    .Object@names <- names
    validObject(.Object)
    return(.Object)
})
#' @export
format.ImportMethodsFrom <- function(x, ...){
    paste0('importMethodsFrom('
          , namespace_quote(x@package), ','
          , namespace_quote(x@names), ')')
}
if(FALSE){#@testing Class: ImportMethodsFrom
    expect_error(import_methods_from())
    expect_error(import_methods_from('pkg'))
    expect_error(import_methods_from('pkg', ''))

    val <- import_methods_from('pkg', 'my_method')
    expect_is(val, 'ImportMethodsFrom')
    expect_equal(val@package, 'pkg')
    expect_equal(val@names, 'my_method')

    val <- import_methods_from('pkg', c('method1', 'method2'))
    expect_is(val, 'ImportMethodsFrom')
    expect_equal(val@package, 'pkg')
    expect_equal(val@names, c('method1', 'method2'))

    expect_identical( format(val)
                    , c( "importMethodsFrom(pkg,method1)"
                       , "importMethodsFrom(pkg,method2)"))
}

namespace_quote <- function(x){
    if (length(x)==0L) return(character(0L)) else
    ifelse(grepl("^[a-zA-Z.][a-zA-Z0-9_.]*$", x), x, shQuote(x, 'cmd'))
}
if(FALSE){#@testing
    expect_identical(namespace_quote(character(0)), character(0))
    expect_identical( namespace_quote(c('hi', 'hello world'))
                    , c('hi', '\"hello world\"')
                    )
}

# Collection Classes -------
## Export Collections ======

setRefVector( 'Export')
setRefVector( 'ExportPattern')
setRefVector( 'ExportS3method')
setRefVector( 'ExportS4methods')
setRefVector( 'ExportS4class')

pass.format <- function(x, ...){
    sort(as.character(unlist(purrr:::map(x$., format, ...))))
}
setMethod('format', "RefVector(Export)"         , pass.format)
setMethod('format', "RefVector(ExportPattern)"  , pass.format)
setMethod('format', "RefVector(ExportS3method)" , pass.format)
setMethod('format', "RefVector(ExportS4methods)", pass.format)
setMethod('format', "RefVector(ExportS4class)"  , pass.format)

#' @export
exports <- setRefClass('ExportEntries',
    fields = c( names    = "RefVector(Export)"
              , patterns = "RefVector(ExportPattern)"
              , S3       = "RefVector(ExportS3method)"
              , S4       = "RefVector(ExportS4methods)"
              , Classes  = "RefVector(ExportS4class)"
              ),
    methods = list(
        add = function(ex){
            if (is(ex, "Export"))             names$add(ex) else
            if (is(ex, "ExportPattern"))   patterns$add(ex) else
            if (is(ex, "ExportS3method"))        S3$add(ex) else
            if (is(ex, "ExportS4methods"))       S4$add(ex) else
            if (is(ex, "ExportS4class"))    Classes$add(ex) else
            doc_error(._("Don't know how to add a %s.", dQuote(class(ex))))
        },
        initialize=function(...){
            for (i in seq_len(...length()))
                add(...elt(i))
        })
    )
setMethod('format', 'ExportEntries', function(x, ...){
    c( sort(format(x$S3, ...))
     , sort(format(x$patterns, ...))
     , sort(format(x$names, ...))
     , sort(format(x$Classes, ...))
     , sort(format(x$S4, ...))
     )
})
if(FALSE){#@testing
    val <- new( 'RefVector(ExportS3method)'
              , export_s3method('c', 'Rd')
              , export_s3method('c', 'ArgumentList')
              )
    expect_is(val, 'RefVector(ExportS3method)')
    expect_length(val, 2L)
    expect_identical(format(val), c("S3method(c,ArgumentList)", "S3method(c,Rd)"))

    export.list <- exports( export("my name")
                          , export_s3method('c', 'Rd')
                          , export_s3method('c', 'ArgumentList')
                          , export_pattern("^doc_")
                          , export_s4methods("initialize")
                          , export_class("Documentation")
                          )
    expect_is(export.list, 'ExportEntries')
    expect_identical( format(export.list)
                    , c( "S3method(c,ArgumentList)"
                       , "S3method(c,Rd)"
                       , "exportPattern(\"^doc_\")"
                       , "export(\"my name\")"
                       , "exportClasses(Documentation)"
                       , "exportMethods(initialize)"
                       ))

    expect_error( export.list$add(import('documentation'))
                , "Don't know how to add")
}


## ImportCollections ======
#' @export
# exports <- setRefVector( 'ExportEntry', 'ExportEntries')
get_packages <- function(){as.character(unlist(purrr::map(., slot, 'package')))}
get_names <- function(){as.character(unlist(purrr::map(., slot, 'names')))}
no_duplicate_names <- function(){
    validate_that( !anyDuplicated(get_names())
                 , msg = "Cannot contain duplicate names.")
}
wrap_validate <- function(){
    valid <- validate()
    if (isTRUE(valid)) return(valid) else
    return(s(FALSE, msg=valid))
}
vec.methods <- list( get_packages = get_packages
                   , get_names    = get_names
                   , validate     = no_duplicate_names
                   , is_valid     = wrap_validate
                   )
rv_import <- setRefVector( 'Import'
  , methods=list( get_packages=get_packages
                , validate = function()validate_that(!anyDuplicated(get_packages()))
                , is_valid = wrap_validate
                ))
rv_import_from    <- setRefVector( 'ImportFrom'       , methods=vec.methods)
rv_import_classes <- setRefVector( 'ImportClassesFrom', methods=vec.methods)
rv_import_methods <- setRefVector( 'ImportMethodsFrom', methods=vec.methods)
rm(get_packages, get_names, no_duplicate_names, vec.methods)

setMethod('format', "RefVector(Import)"           , pass.format)
setMethod('format', "RefVector(ImportFrom)"       , pass.format)
setMethod('format', "RefVector(ImportClassesFrom)", pass.format)
setMethod('format', "RefVector(ImportMethodsFrom)", pass.format)

setMethod('names', 'RefVector(Import)', function(x)purrr::map_chr(x$., slot, 'package'))

imports <- setRefClass('ImportEntries',
    fields = c( packages = "RefVector(Import)"
              , objects  = "RefVector(ImportFrom)"
              , classes  = "RefVector(ImportClassesFrom)"
              , methods  = "RefVector(ImportMethodsFrom)"
              ),
    methods = list(
        add = function(ex){
            if (is(ex, "Import"))            packages$add(ex) else
            if (is(ex, "ImportFrom"))         objects$add(ex) else
            if (is(ex, "ImportClassesFrom"))  classes$add(ex) else
            if (is(ex, "ImportMethodsFrom"))  methods$add(ex) else
            doc_error(._("Don't know how to add a %s.", dQuote(class(ex))))
        },
        check_no_extraneous_import_from = function(){
            pkgs <- objects$get_packages()
            x <- pkgs %in% packages$get_packages()
            if (!any(x)) return(TRUE)
            msg <- ngettext( sum(x)
                           , "package %s already appears as an import, it"
                           , "packages %s already appear a imports, they"
                           ) %<<% "should not appear in any importFrom calls."
            s(FALSE, msg=._(msg, comma_list(pkgs[x])))
        },
        validate = function(...){
            validate_that( objects$is_valid()
                         , packages$is_valid()
                         , classes$is_valid()
                         , methods$is_valid()
                         , check_no_extraneous_import_from()
                         )
        },
        is_valid = wrap_validate,
        get_packages = function(){
            sort(unique(c(packages$get_packages()
                         , objects$get_packages()
                         , classes$get_packages()
                         , methods$get_packages()
                         )))
        },
        initialize=function(...){
            for (i in seq_len(...length()))
                add(...elt(i))
        })
    )
setMethod('format', 'ImportEntries', function(x, ...){
    c( sort(format(x$packages))
     , sort(format(x$objects))
     , sort(format(x$classes))
     , sort(format(x$methods))
     )
})
if(FALSE){#@testing
    bare <- new('RefVector(Import)')
    expect_valid(bare)

    packages <- new('RefVector(Import)', import('utils'), import('methods'))
    expect_identical( sort(names(packages)), c('methods', 'utils'))
    expect_identical(format(packages), c('import(methods)', 'import(utils)'))

    x <- import_from('utils', c('head', 'tail'))
    objects <- new('RefVector(ImportFrom)', x)
    expect_identical(format(objects), c('importFrom(utils,head)', 'importFrom(utils,tail)'))

    import.list <- imports( import('utils'), import('methods')
                          , x
                          , import_classes_from('documentation', c('Documentation', 'function-Documentation'))
                          , import_methods_from('documentation', c('doc_get_aliases', 'doc_get_name'))
                          )
    expect_is(import.list, 'ImportEntries')
    expect_equal( import.list$validate()
                , "package utils already appears as an import," %<<%
                  "it should not appear in any importFrom calls.")

    expect_identical( format(import.list)
                    , c( 'import(methods)', 'import(utils)'
                       , 'importFrom(utils,head)', 'importFrom(utils,tail)'
                       , 'importClassesFrom(documentation,\"function-Documentation\")'
                       , 'importClassesFrom(documentation,Documentation)'
                       , 'importMethodsFrom(documentation,doc_get_aliases)'
                       , 'importMethodsFrom(documentation,doc_get_name)'
                       ))

    expect_error( import.list$add(export('Documentation'))
                , "Don't know how to add")

    expect_identical( import.list$get_packages()
                    , c('documentation', 'methods', 'utils'))

    ilist <- imports( import_from('purrr', '%||%')
                    , import_from('rlang', '%||%')
                    )
    ilist$objects$get_names()

    expect_equal(ilist$validate(), "Cannot contain duplicate names.")

    expect_true(ilist$check_no_extraneous_import_from())
}


namespace <-
setRefClass('NamespaceEntries',
    fields = c( imports='ImportEntries'
              , exports='ExportEntries'
              ),
    methods = list(
        add = function(entry){
            if (is(entry, 'ExportEntry')) exports$add(entry) else
            if (is(entry, 'ImportEntry')) imports$add(entry) else
            doc_error(._("Don't know how to add a %s.", dQuote(class(entry))))
        },
        initialize = function(...){
            for (i in seq_len(...length()))
                add(...elt(i))
        },
        validate = function(){
            validate_that( imports$is_valid()
                         , exports$is_valid()
                         )
        },
        is_valid = wrap_validate
        )
    )
setMethod('format', 'NamespaceEntries', function(x, ...){
    c( format(x$exports)
     , format(x$imports)
     )
})
if(FALSE){#@testing
    my.ns <- namespace( export("my name")
                      , import('utils'), import('methods')
                      , export_s4methods("initialize")
                      , export_s3method('c', 'ArgumentList')
                      , export_pattern("^doc_")
                      , import_classes_from('documentation', c('Documentation', 'function-Documentation'))
                      , import_from('utils', c('head', 'tail'))
                      , import_methods_from('documentation', c('doc_get_aliases', 'doc_get_name'))
                      , export_class("Documentation")
                      , export_s3method('c', 'Rd')
                      )
    expect_is(my.ns, 'NamespaceEntries')
    expect_identical( format(my.ns)
                    , c( "S3method(c,ArgumentList)"
                       , "S3method(c,Rd)"
                       , "exportPattern(\"^doc_\")"
                       , "export(\"my name\")"
                       , "exportClasses(Documentation)"
                       , "exportMethods(initialize)"
                       , 'import(methods)', 'import(utils)'
                       , 'importFrom(utils,head)', 'importFrom(utils,tail)'
                       , 'importClassesFrom(documentation,\"function-Documentation\")'
                       , 'importClassesFrom(documentation,Documentation)'
                       , 'importMethodsFrom(documentation,doc_get_aliases)'
                       , 'importMethodsFrom(documentation,doc_get_name)'
                       ))
    expect_error( my.ns$add('Hallo, my name is Inigo Montoya...')
                , "Don't know how to add")

    my.ns$validate()


}
