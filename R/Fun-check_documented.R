
setGeneric("is_complete", valueClass="logical", function(doc){
    stop("is_complete is intended to be used with documentation objects only.")
})
setMethod("is_complete", "Documentation", function(doc){
    warning("is_complete is not implemented for documentation class ", class(doc))
    invisible(TRUE)
})

    
is_documented <- 
    function( name #< name of object to check
            , env  #< environment containing object identified by `name`
            , ...  #< other arguments, currently ignored.
            , complete = TRUE #< check if documentation is complete.
            ){
    object <- get(name, envir = env, inherits = FALSE)
    tryCatch({
                doc <- documentation(object, name=name)
                if (complete && !is_complete(doc)) doc_incomplete()
                return(TRUE)
             }
            # , 'documentation-error-dnf' = function(e) 
    
    , error = function(e) structure(FALSE, msg = e$message, reason=class(e)[[1]])
    , warning = function(w) structure(FALSE, msg = w$msg, reason=class(e)[[1]])
    )
}
if(FALSE){#@testing
    test_function <- function(x){}
    
    expect_false( is_documented('test_function', environment()))
    expect_equal( attr(is_documented('test_function', environment()), 'reason')
                , "documentation-error-dnf"
                )
    
    attr(test_function, "documentation") <- "An invalid documentation"
    expect_false( is_documented('test_function', environment()))
    expect_equal( attr(is_documented('test_function', environment()), 'reason')
                , "documentation-error-invalid"
                )
    documentation(test_function) <- 
        function_documentation('test_function'
                              , title = 
                                  "A function for testing documentation."
                              )
    expect_true(is_documented('test_function', environment(), complete=FALSE))
}



#' Check all exports are documented
#' 
#' Check that all exports for a given package are documented.
check_exports <- 
function( pkg='.'   #< package name to check.
        , exclude = "\\..*" #< regular expressions to exclude from checking.
        , ...
        ){
    pkg <- devtools::as.package(pkg, create=FALSE)
    env <- pkg_env(pkg)
    if (is.null(env))
        devtools::load_all(pkg)
    ns <- asNamespace(pkg$package)
    exports <- getNamespaceExports(ns)
    for(epat in exclude) 
        exports <- grep(exclude, exports, invert=TRUE, value=TRUE)
    is.documented <- lapply(exports, is_documented, env=ns, ...)
    
    Reason <- sapply( sapply(is.documented, attr, 'reason')
                    , function(.x) if (is.null(.x)) '' else .x)
    Message <- sapply( sapply(is.documented, attr, 'msg')
                     , function(.x) if (is.null(.x)) NA_character_ else .x)
    if (default.stringsAsFactors())
        Reason <- as.factor(Reason)
    structure( data.frame( Name       = exports
                         , Documented = sapply(is.documented, isTRUE)
                         , Reason     = Reason
                         , Message    = Message
                         , stringsAsFactors = FALSE
                         )
             , names = ._(c('Name', 'Documented', 'Reason', 'Message'))
             , class = c("Documentation Check Results", "data.frame")
             , package = packageName(ns)
             )
}
if(FALSE){
    pkg <- 'documentation'
    
    object <- check_exports('.', complete=FALSE)
    
    expect_is(object, "Documentation Check Results")
    expect_is(object, "data.frame")
}


#' @export
`print.Documentation Check Results` <- 
function(object,...){
    if (!is.null(pkg <- attr(object, 'package')))
        cat("Documentation check summary for package" %<<% pkg %<<<% "\n\n")
    
    undocumented  <- object$Reason == "documentation-error-dnf"
    incomplete    <- object$Reason == "documentation-warning-incomplete"
    invalid       <- object$Reason == "documentation-error-invalid"
    other.error   <- object$Reason == "documentation-error"
    other.warning <- object$Reason == "documentation-warning"
    
    cat(c( ._("* Total elements: %d", nrow(object))
         , ._("* Documented: %d"    , sum(object$Documented))
         , if (any(undocumented)) ._("* Undocumented: %d"      , sum(undocumented))
         , if (any(incomplete))   ._("* Incomplete: %d"        , sum(incomplete))
         , if (any(invalid))      ._("* Invalid: %d"           , sum(invalid))
         , if (any(other.error))  ._("* Has other errors: %d"  , sum(other.error))
         , if (any(other.warning))._("* Has other warnings: %d", sum(other.warning))
         ) %\%
        "" %\%
        ._("Percent complete: %2.1f%%", mean(object$Documented)*100) %\%
        "" %\%
        c(  if (all(object$Documented)){
                ._("Congratulations! You are a documentation rock star.")
            }
         ,  if (any(undocumented)){
                ._("Undocumented Objects:|") %\%
                collapse_nl(
                    strwrap( comma_list(object[object$Reason == 'documentation-error-dnf', 1L])
                           , width = 0.8 * getOption("width")
                           , indent = 2, exdent = 2
                           )
                )
            }
         ,  if (any(incomplete)){
                "Objects with incomplete documentation:|" %\%
                collapse_nl(
                    strwrap( comma_list(object[object$Reason == 'documentation-warning-incomplete', 1L])
                           , width = 0.8 * getOption("width")
                           , indent = 2, exdent = 2
                           )
                )
            }
         ,  if (any(invalid)){
                "Objects with invalid documentation:|" %\%
                collapse_nl(
                    strwrap( comma_list(object[object$Reason == 'documentation-error-invalid', 1L])
                           , width = 0.8 * getOption("width")
                           , indent = 2, exdent = 2
                           )
                )
            }
         )
        )
}
if(FALSE){#@testing
    is_documented('documentation', asNamespace("documentation"), complete =FALSE)
    is_documented('documentation<-', asNamespace("documentation"), complete =FALSE)

    
}