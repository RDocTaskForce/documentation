#' @include Classes.R
#' @include Fun-extract_documentation.R


extract_documentation.classGeneratorFunction <-
function( object
        , ...
        , envir = environment(object)
        , srcfile    = NULL
        , pd         = NULL
        , roxy.block = NULL
        , markdown   = FALSE
        , name       = substitute(object)
        ){
    Class <- getClass(object@className)
    if (exists(name, envir = envir$`.__T__initialize:methods`)){
        init <- get(object@className, envir$`.__T__initialize:methods`)
        src <- utils::getSrcref(init)
        if (is.null(srcfile))
            srcfile <- utils::getSrcFilename(src, full.names = TRUE)
    }
    roxy.block <- .get_roxy_block(Class, srcfile=srcfile, ...)

    docs <- .construct_documentation.function(object, roxy.block, pd, name=name)
    if (.is_undefined(doc_get_name(docs))) docs@name <- as.name(name)


    mname <- classMetaName(object@className)
    get(mname, envir)
}
if(F){# Development
    debug(extract_documentation.classGeneratorFunction)
    object <- arg_
    name <- 'arg_'
    envir <- asNamespace("documentation")


    envir$`.__T__initialize:methods` %>% ls(all=TRUE)
    init <- envir$`.__T__initialize:methods`$`arg-Documentation`

    class(init)
    str(init)

    src <- getSrcref(init)
    srcfile <- utils::getSrcFilename(src,T,T)

    pd <- get_parse_data(src)
    file <- attr(pd, 'srcfile')$filename
    debug(.get_roxy_block)
    roxy.block <- .get_roxy_block(Class, srcfile = srcfile, envir=envir)

    file.exists(file)
    con <- file(file, 'r')
    getSrcLines(file, min(pd_start_line(pd$id, pd)), max(pd_end_line(pd$id, pd)))

    as.character(src)

    init@.Data
}

