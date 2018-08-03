
.document.allways.exclude <-
    c( '.__DEVTOOLS__'
     , '.__NAMESPACE__.'
     , '.__S3MethodsTable__.'
     , '.packageName'
     , '.requireCachedGenerics'
     , '.S3MethodsClasses'
     , '.documentation.exclude'
     , '.documentation.exclude.patterns'
     , '.documentation.exclude.classes'
     )

.document.allways.exclude.patterns <-
    c( "^\\.__C__"     #< Class Details
     , "^\\.__T__"     #< S4 Methods
     , "^\\.__Documentation" #< Documentation objects.
     )
.document.default.exclude.objects  <- character(0)
.document.default.exclude.patterns <- character(0)
.document.default.exclude.classes <-
    c( 'logical', 'integer', 'numeric', 'complex', 'character', 'raw', 'factor'
     )

.document.default.excludes <- list( objects = .document.default.exclude.objects
                                  , patterns= .document.default.exclude.patterns
                                  , classes = .document.default.exclude.classes
                                  )

#' Document all objects in an environment
#'
#' Iterate through all objects in an environment and extract documentation.
#'
#'
#' @export
#' @aliases .documentation.exclude .documentation.exclude.patterns
#'          .documentation.exclude.classes
document_env <-
function( envir = parent.frame() #< environment with objects to documents.
        , exclude = NULL         #< Objects to not document, see details.
        , ...                    #< passed to extract_documentation.
        , only.exports = FALSE   #< if envir is a \link[asNamespace]{namespace}
                                 #< only document the exports?
        ){
    #' @details
    #'
    #' if `document_env` will attempt to
    object.names <-
        if (isNamespace(envir) && only.exports)
            getNamespaceExports(envir)
        else
            objects(envir=envir, all=TRUE)
    if(is.character(exclude)){
        #' The `exclude` parameter is assumed to be a vector of patterns to
        #' to identify objects to exclude if given a character vector.
        .exclude <- list( objects  = .document.default.exclude.objects
                        , patterns = exclude
                        , classes  = .document.default.exclude.classes
                        )
    } else if (is(exclude, 'list')) {
        #' It can also be passed a vector with elements
        #' objects, patterns, and classes to identify
        #' specific objects, patterns as above, and classes of objects
        #' to exclude from documentation extraction, respectively.
        #' If classes is missing it will be assumed to be the default of
        #' excluding atmic vectors plus factors, however if provided
        #' those classes may be included.
        if (!all(names(exclude) %in% c('objects', 'patterns', 'classes')))
            doc_error(._( "Argument `exclude` to document_env() " %<<%
                          "has incorrect names.  If provided as a list" %<<%
                          "exclude is expected to have named elements" %<<%
                          "objects, patterns,and classes"))
        if (any(. <- !sapply(exclude, is.character)))
            doc_error(._( ifelse(sum(.)==1, "Element %s", "Elements %s") %<<%
                          "of the `exclude` argument to document_env()" %<<%
                          "are not character vectors.", comma_list(names(which(.)))
                        ))
        if (is.null(exclude$objects )) exclude$objects  <- .document.default.exclude.objects
        if (is.null(exclude$patterns)) exclude$patterns <- .document.default.exclude.patterns
        if (is.null(exclude$classes )) exclude$classes  <- .document.default.exclude.classes
        .exclude <- exclude
    } else if (is.null(exclude)) {
        #'
        #' If the `exclude` parameter is missing or `NULL`, the default,
        #' document_env will search for objects that can be defined in the
        #' package namespace:
        #'
        #' * `.documentation.exclude` which gives the list of exact names to
        #'   exclude from extracting and checking documentation, and
        #' * `.documentation.exclude.patterns` which gives the list of patterns
        #'   which exclude objects from documentation extraction and checking.
        #' * `.documentation.exclude.classes` which lists classes that will be
        #'   excluded from attempting to extract documentation for.
        #'   By default all \link[base:vector]{atmic} types, plus factor.
        .exclude <- mget( c( '.documentation.exclude'
                           , '.documentation.exclude.patterns'
                           , '.documentation.exclude.classes'
                           )
                        , envir=envir, mode='character'
                        , ifnotfound = .document.default.excludes
                        )
        names(.exclude) <- c('objects', 'patterns', 'classes')
        if (!is.character(.exclude$objects))
            doc_error(".documentation.exclude must be a character vector.")
        if (!is.character(.exclude$patterns))
            doc_error(".documentation.exclude.patterns must be a character vector.")
        if (!is.character(.exclude$classes))
            doc_error(".documentation.exclude.patterns must be a character vector.")
    } else {
        doc_error("Bad argument `exclude` to document_env().")
    }

    all.excludes <- c( .document.allways.exclude
                     , .exclude$.documentation.exclude
                     )
    all.patterns <- c( .document.allways.exclude.patterns
                     , .exclude$.documentation.exclude.patterns
                     )
    object.names <- setdiff(object.names, all.excludes)
    for (pat in all.patterns)
        object.names <- grep(pat, object.names, value = TRUE, invert = TRUE)
    for (on in object.names) if (!is_documented(on, envir, complete=FALSE))
        if (!inherits(envir[[on]], what=.exclude$classes))
            documentation(envir[[on]]) <-
                extract_documentation(envir[[on]], ..., name = as.name(on))
    invisible(envir)
}
if(F){# development
    env <- new.env()
    env$.packageName <- "documentation-testing"
    sys.source( system.file("examples", "example_function1.R", package='documentation')
              , envir = env, keep.source=TRUE )
    sys.source( system.file("examples", "standardGeneric.R", package='documentation')
              , envir = env, keep.source=TRUE )

    document_env(env)
    expect_true(is_documented('example_function1', env, complete=FALSE))
    expect_true(is_documented('example_generic', env, complete=FALSE))
}


#' Document an object
#'
#' This extracts documentation from source files,
#' overwriting any explicit sections provided in the call,
#' then stores the documentation back to the original object
#' in it's original namespace.
setGeneric('document', signature = 'object',
function( object #< object to document
        , ...    #< explicit elements to override.
        , name = deparse(substitute(object)) #< name of the object.
                 #< Besided being included in documentation, is also used to
                 #< identify the environment the object originated in.
        , envir = NULL #< environment to find object in.
        , create.new = TRUE #< If the original object cannot be overwritten,
                            #< create a new object in the global environment.
        ){
    if (is.null(envir)) envir <- parent.frame()
    while (!identical(envir, emptyenv())){
        if (exists(name, envir) && identical(get(name, envir = envir), object)) break
        else envir <- parent.frame()
    }
    if (identical(envir, emptyenv()))
        doc_error("Object '%s' cannot be found.", name)
    if (environmentIsLocked(envir)){
        ename <- environmentName(envir)
        msg <- ._("Environment%*s containing object '%s' is locked."
                 , nchar(ename)+1, ename, name) %<<%
               ._("Cannot attach documentation in original environment.")
        if (create.new) doc_error(msg, type='env_locked')
        else{
            msg <- msg %<<% ._("Assigning copy in global environment instead.")
            doc_warning(msg, type='env_locked')
            envir <- globalenv()
        }
    }
    docs <- tryCatch( extract_documentation(object)
                    , 'documentation-error-no_src' = function(e)
                        new('Documentation')
                    )
    overrides <- list(...)
    for (i in seq_along(overrides)){
        element <- names(overrides)[[i]]
        if (element %in% slotNames(docs))
            slot(docs, element) <- overrides[[i]]
    }
    documentation(object) <- docs
    if (isS4(object))
        class(object) <- c('documented-' %<<<% class(object)[[1]], class(object))
    assign(name, object, envir = envir)
})
if(FALSE){#@testing
    env <- environment()
    sys.source( system.file("examples", "example_function1.R", package='documentation')
              , envir = env, keep.source=TRUE )

    expect_false(is_documented('example_function1', env))
    result <- document(example_function1)

    expect_identical(result, example_function1)
    expect_true(is_documented('example_function1', env, complete=FALSE))

    expect_identical( documentation(example_function1)@title
                    , "This is the title")

    expect_identical( documentation(example_function1)@arguments
                    , ArgumentList( x = arg(x, "inline documentation for x")
                                  , y = arg(y, "explicit documentation for y")
                                  ))
    expect_identical( documentation(example_function1)@value
                    , FormattedText("explicit return."))

    expect_identical( documentation(example_function1)@name
                    , as.name("example_function1"))
}

#' @describeIn document  Document a package documentation style.
#'
#' When document is provided a character string it is assumed
#' to be the path to a package.
#'
#' @note character vectors do not carry any class information
#' and thus documentation cannot be extracted
setMethod('document', 'character',
function( object = '.'  #< the package to document.
        , ...           #< passed on to document_env
        ){
    pkg <- devtools::as.package(object)
    devtools::load_all(pkg, export_all=FALSE)
    ns <- asNamespace(pkg$package)
    document_env(ns, ...)
})
