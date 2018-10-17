#' @include setup.R
#' @include utils.R

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
                    , c( 'importClassesFrom(class1)'
                       , 'importClassesFrom("class 2")'
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


