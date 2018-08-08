#' @include Classes.R

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
    , error = function(e) structure(FALSE, msg = e$message, reason=class(e)[[1]])
    , warning = function(w) structure(FALSE, msg = w$msg, reason=class(w)[[1]])
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
    env <- devtools::pkg_env(pkg)
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
             , package = utils::packageName(ns)
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
function(x,...){
    if (!is.null(pkg <- attr(x, 'package')))
        cat("Documentation check summary for package" %<<% pkg %<<<% "\n\n")

    reasons <- setdiff(unique(x[[3L]]), c('', NA))
    reason.counts <- sapply(reasons, function(r)sum(x[[3L]] == r))

    cat(c( ._("* Total elements: %d", nrow(x))
         , ._("* Documented: %d"    , sum(x[[2L]]))
         , paste0("* ", names(reason.counts), ": ", reason.counts)
         ) %\%
        "" %\%
        ._("Percent complete: %2.1f%%", mean(x$Documented)*100)
    )
    if (length(reasons)==0)
        cat("\n", ._("Congratulations! You are a documentation rock star."))
    else for (r in reasons){
        cat( "" %\% r %<<<% ":" %\%
             collapse_nl(
                strwrap( comma_list(x[x[[3L]] == r, 1L])
                       , width = 0.8 * getOption("width")
                       , indent = 2, exdent = 2
                       )
             )
           )
    }
}