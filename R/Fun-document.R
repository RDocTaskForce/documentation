
.document.allways.exclude <-
    c( '.__DEVTOOLS__'
     , '.__NAMESPACE__.'
     , '.__S3MethodsTable__'
     , '.packageName'
     , '.requireCachedGenerics'
     , '.S3MethodsClasses'
     , '.documentation.exclude.patterns'
     , '.documentation.exclude'
     , ''
     )
.document.allways.exclude.patterns <-
    c( "^\\.__C__"     #< Class Details
     , "^\\.__T__"     #< S4 Methods
     , "^\\.__Documentation" #< Documentation objects.
     )


#' Document all objects in an environment
#'
#' Itterate through all objects in an environment and extract documentation.
#'
document_env <-
function( envir = parent.frame() #< environment with objects to documents.
        , exclude = NULL #< pattern(s) indicating objects to not document.
        , ...
        ){
    object.names <- objects(envir=envir, all=TRUE)
    if (is.null(exclude)) {
        #' If the `exclude` parameter is missing or `NULL`, the default,
        #' document_env will search for two objects that can be defined in the
        #' package namespace:
        #'
        #' * `.documentation.exclude` which gives a list of exact names to
        #'   exclude from extracting and checking documentation, and
        #' * `.documentation.exclude.patterns` which gives a list of patterns
        #'   which exclude objects from documentation extraction and checking.
        .exclude <- mget( c('.documentation.exclude', '.documentation.exclude.patterns')
                        , envir=envir, mode='character'
                        , ifnotfound=list(character(0), character(0))
                        )
        if (!is.character(.exclude$.documentation.exclude))
            doc_error(".documentation.exclude must be a character vector.")
        if (!is.character(.exclude$.documentation.exclude.patterns))
            doc_error(".documentation.exclude.patterns must be a character vector.")
    } else {
        .exclude <- list( .documentation.exclude          = character(0)
                        , .documentation.exclude.patterns = character(0)
                        )
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
    for (on in object.names)
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
        if (exists(name, envir)) break
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

