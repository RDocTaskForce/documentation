#' @import parsetools

.roxy.namespace <- roxygen2::roclet_tags( roxygen2::roclet('namespace'))
.roxy.rd <- roxygen2::roclet_tags( roxygen2::roclet('rd'))

.mask.namespace <- 
    structure( replicate( n = length(.roxy.namespace)
                        , expr = function(x)NULL
                        , simplify = FALSE
                        )
             , names = names(.roxy.namespace)
             )
.mask.collate <- list(
    include = function(x)NULL
)

.new.tags <- list(
    internal = function(x){
        if(x$val != "")
            roxygen2::roxy_tag_warning(x, "cannot have arguments.")
        else
            roxygen2::roxy_tag("keywords", "internal", x$file, x$line)
    }
)


.roxy.registry <- c( .roxy.rd
                   , .mask.namespace
                   , .mask.collate
                   , .new.tags
                   )

.get_roxy_block <- 
function( object
        , srcfile = NULL
        , registry=.roxy.registry
        , options=list()
        ){
    if(is.null(srcfile)){
        srcref <- utils::getSrcref(object)
        srcfile <- utils::getSrcFilename(srcref, full.names = TRUE, unique = TRUE)
    }
    env <- new.env(parent = environment(object))
    roxy.blocks <- roxygen2::parse_file( srcfile, env=env, registry = registry
                                       , global_options=options)
    for (block in roxy.blocks){
        # call <- attr(block, 'call')
        # location <- attr(block, 'location')
        if (identical(attr(block, 'object')$value, object)) {
            return(block)
        }
    }
}

extract_documentation <- function(object, ...)UseMethod('extract_documentation')

extract_documentation.function <- function(object, ...){
    pd <- get_parse_data(object)
    srcref <- utils::getSrcref(object)
    name <- substitute(object)
    if (!is.null(srcref)) {
        srcfile <- utils::getSrcFilename(srcref, full.names = TRUE, unique = TRUE)
        if (!is.null(srcfile)) {
            env <- new.env(parent = environment(object))
            roxy.blocks <- roxygen2::parse_file(srcfile, env, registry = .roxy.registry)
            roxy.block <- NULL
            for (i in seq_along(roxy.blocks)){
                block <- roxy.blocks[[i]]
                call <- attr(block, 'call')
                location <- attr(block, 'location')
                if (identical(attr(block, 'object')$value, object)) {
                    roxy.block <- block
                    break
                }
            }
            if (is.null(roxy.block)) {
                docs <- new('function-Documentation')
            } else {
                docs <- as(roxy.block, 'function-Documentation')
                alias <- attr(block, 'object')$alias
                if (!is.null(alias))
                    docs@name <- as.name(alias)
            }
        }
    }
    if (.is_undefined(docs@name))
        docs@name <- substitute(object)
    root <- pd_all_root_ids(pd)
    while (pd_is_assignment(root, pd)) root <- pd_get_assign_value_id(root, pd)
    stopifnot(pd_is_function(root, pd))
    
    args <- pd_get_function_arg_variable_ids(root, pd)
    for (arg in args){
        relative.comments <- pd_get_function_arg_associated_comment_ids(arg, pd)
        if (length(relative.comments)){
            text <- strip_doc_comment_leads(pd_text(relative.comments, pd))
            if (length(text) > 1L)
                text <- paste(text, collapse=' ')
            if (text != ''){
                arg.name <- pd_text(arg, pd)
                if (arg.name %in% names(docs@arguments))
                    stop( "documentation for ", docs@name
                        , "already contains documentation for argument ", arg.name, "\n"
                        , "error at ",  pd_filename(pd), ":", pd_start_line(arg, pd), ":", pd_start_col(arg, pd)
                        )
                docs@arguments[[arg.name]] <- 
                    arg_( name = arg.name
                        , description = text
                        )
                
            }
        }
    }
    docs
}
if(FALSE){#@testing
    test.file <- system.file("examples", "example_function1.R", package='documentation')
    source( test.file, keep.source=TRUE)
    expect_true(exists("example_function1"))
    pd <- get_parse_data(example_function1)

    object <- example_function1
        
    docs <- extract_documentation(example_function1)
    
    expect_is(docs, 'function-Documentation')
    expect_equal( docs@arguments$x
                , arg(x, "inline documentation for x")
                )
    expect_equal( names(docs@arguments)
                , names(formals(example_function1))
                )
    
    expect_error(documentation(example_function1), 'Documentation not found!')
    
    
}

extract_documentation.standardGeneric <- function(object, markdown=FALSE, ...){
    name <- substitute(object)
    default.method <- attr(object, 'default')
    srcref <- utils::getSrcref(default.method)
    stopifnot(inherits(srcref, 'srcref'))
    srcfile <- utils::getSrcFilename(srcref, full.names = TRUE, unique = TRUE)
    pd <- get_parse_data(srcref)
    has.roxy <- any(pd$token == "ROXYGEN_COMMENT")
    docs <- if (has.roxy){
        roxy.block <- .get_roxy_block(object, srcfile, options=list(markdown=markdown))
        docs <- as(roxy.block, 'function-Documentation')
        alias <- attr(roxy.block, 'object')$alias
        if (!is.null(alias))
            docs@name <- as.name(alias)
        docs
    } else new('function-Documentation')
    if (.is_undefined(docs@name))
        docs@name <- name
    if (is.na(docs@value))
        docs@value <- FormattedText("Methods are restricted to returning an object of class" %<<% 
                                    object@valueClass %<<<% "." )
    docs
}
if (FALSE) {#@testing 
    
    source(system.file("examples", "standardGeneric.R", package = "documentation"), keep.source = TRUE)
    docs <- extract_documentation(example_generic)
    
    expect_identical(docs@name, as.name("example_generic"))
    expect_identical(docs@title, "Example Generic Function")
    expect_identical(docs@description
                    , "This is an example of an S4 standardGeneric " %\% 
                      "documentation.  This is to be used for testing" %\% 
                      "of the functions in the documentation package."  %>%
                        FormattedText())
    expect_identical(docs@value, FormattedText("Methods are restricted to returning an object of class logical."))
}