#' @include Classes.R
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
        , ...
        , srcfile  = NULL #< File containing source for object.
        , envir    = NULL #< Environment containing object.
        , registry =.roxy.registry #< registry of roxy
        , options  = list()
        ){
    if (is.null(srcfile)) {
        srcref <- utils::getSrcref(object)
        if (is.null(srcref)) doc_no_src()
        srcfile <- utils::getSrcFilename(srcref, full.names = TRUE, unique = TRUE)
    }
    if (is.null(envir)) {
        envir <- if (is.null(environment(object))) parent.frame()
                 else environment(object)
    }
    env <- new.env(parent = envir )
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
if(FALSE){#@testing
    test.file <- system.file("examples", "example_character.R", package='documentation')
    sys.source( test.file, keep.source=TRUE)

    expect_error(.get_roxy_block(example_character), class='documentation-error-no_src')
    block <- .get_roxy_block(example_character, srcfile=test.file)

    expect_is(block, 'roxy_block')
    expect_equal(block$title, "An example character vector")
    expect_equal(attr(block, 'object')$alias, 'example_character')
}

#' @export
get_parse_data.roxy_block <- function(x,...){
    get_parse_data(srcref( srcfile(attr(x, 'filename'))
                         , attr(x, 'location')
                         ))
}


.construct_documentation.function <-
function( object
        , roxy.block            #< roxy_block for object
        , pd                    #< parse.data for object
        , doc.class = 'function-Documentation'
        , name = deparse(substitute(object))
        ){
    if (is.null(pd))
        pd <- get_parse_data(object)
    else if(length(pd_all_root_ids(pd)) > 1L)
        doc_error( ._("Bad parse data (multiple root ids found)."), type = "bad_pd")
    id <- attr(pd, 'id')
    if (is.null(id)){
        id <- root <- pd_all_root_ids(pd)
        while (pd_is_assignment(id, pd)) id <- pd_get_assign_value_id(id, pd)
        stopifnot(pd_is_function(id, pd))
    }
    has.roxy     <- inherits(roxy.block, 'roxy_block')
    rel.comments <- pd_all_relative_comment_ids(pd)

    if (!(has.roxy || length(rel.comments))) no_doc_comments()
    docs <- if (has.roxy) as(roxy.block, doc.class)
            else new(doc.class)
    if (length(rel.comments)) {
        associated <- pd_get_relative_comment_associated_ids(rel.comments, pd)
        for (a in unique(associated)){
            if (pd_is_function_arg(a, pd, .check=FALSE)){
                arg.name <- pd_text(a, pd)
                if (arg.name %in% names(docs@arguments))
                    doc_error(._( "documentation for %s already contains" %<<%
                                  "documentation for argument %s"
                                , doc_get_name(docs), arg.name))
                comments <- rel.comments[!is.na(associated) & associated==a]
                text <- strip_doc_comment_leads(pd_text(comments, pd))
                docs@arguments[[arg.name]] <-
                    arg_( name = arg.name
                        , description = collapse(text)
                        )
            } else {
                file <- pd_filename(pd)
                line <- pd_start_line(rel.comments[match(a, associated)], pd)
                doc_warning(._( "orphan relative comment found in %s on line %s"
                              , file, line )
                           , filename = file
                           , line = line
                           , type= "orphan_comment")
            }
        }
    }
    if (length(docs@arguments) > 1L){
        o <- order(match(names(docs@arguments), names(formals(object))))
        #TODO Should I add checking here to see if the arguments are
        #     actually in the function formals?
        #TODO Should I add options for the sorting of the arguments alphabetically?
        docs@arguments <- docs@arguments[o]
    }
    docs
}
if(FALSE){#@testing
    test.file <- system.file("examples", "example_function1.R", package='documentation')
    sys.source( test.file, keep.source=TRUE)
    expect_true(exists("example_function1"))
    pd <- get_parse_data(example_function1)

    roxy.block <- .get_roxy_block(example_function1, srcfile=test.file)

    docs <- .construct_documentation.function(example_function1, roxy.block, pd)
    expect_is(docs, 'function-Documentation')
}

#' Extract documentation for an object from sources.
#' 
#' This generic function extracts documentation from source files.
#' Documentation should be of the form of roxygen comments or relative comments.
#' 
#' @export
extract_documentation <-
function( object   #< Object for which to extract documentation.
        , ...      #< passed on to methods.
        , envir              #< Environment of object.
        , pd         = NULL  #< parse data for object.
        , roxy.block = NULL  #< Roxygen2 block for object.
        , name = substitute(object) #< Name of object in envir.
        ){
    if (!is.null(pd)){
        if (!inherits(pd, 'parse-data'))
            doc_error( ._("Bad parse data (incorrect class)"), type ="bad_pd")
        if (length(pd_all_root_ids(pd)) > 1L)
            doc_error( ._("Bad parse data (multiple root ids found)."), type = "bad_pd")
    }
    if (!is.null(roxy.block)){
        if(!inherits(roxy.block, "roxy_block"))
            doc_error( ._("Bad roxy.block (incorrect class)"), type ="bad_roxy")
        if ( !is.null(alias <- attr(roxy.block, 'object')$alias)
          && !(as.character(name) %in% alias)
           )
            doc_error( ._("Bad roxy.block (%s not in alias)", as.character(name))
                     , type ="bad_roxy")
    }
    UseMethod('extract_documentation')
}

#' @export
extract_documentation.function <-
function( object   #< function to document.
        , ...      #< passed on. Should not be needed when calling directly.
        , pd = NULL #< The parse data for object, and only object.
        , markdown = getOption("documentation::use_markdown", TRUE) #< use markdown?
        , name = substitute(object) #< explicit naming for indirect calling.
        ){
    # srcref <- utils::getSrcref(object)
    # if (is.null(srcref)) doc_no_src(name)
    if (is.null(pd))
        pd <- get_parse_data(object)
    else if(length(pd_all_root_ids(pd)) > 1L)
        doc_error( ._("Bad parse data (multiple root ids found)."), type = "bad_pd")
    id <- attr(pd, 'id')
    if (is.null(id)){
        id <- root <- pd_all_root_ids(pd)
        while (pd_is_assignment(id, pd)) id <- pd_get_assign_value_id(id, pd)
        stopifnot(pd_is_function(id, pd))
    }
    has.roxy     <- any(pd$token == "ROXYGEN_COMMENT")
    rel.comments <- pd_all_relative_comment_ids(pd)

    if (!(has.roxy || length(rel.comments)))
        no_doc_comments(name)
    roxy.block <- .get_roxy_block(object, ..., options=list(markdown=markdown))
    docs <- .construct_documentation.function(object, roxy.block, pd)
    if (.is_undefined(docs@name))
        docs@name <- as.name(name)
    else if (!missing(name) && deparse(docs@name) != deparse(name))
        doc_error( "Name provided does not match source."
                 , provided=name, source = docs@name)
    docs
}
if(FALSE){#@testing extract_documentation.function with example_function1
    test.file <- system.file("examples", "example_function1.R", package='documentation')
    sys.source( test.file, keep.source=TRUE)
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

    expect_error( documentation(example_function1)
                , class = 'documentation-error-dnf')
}
if(FALSE){#@testing extract_documentation.function with example_function2
    test.file <- system.file("examples", "example_function2.R", package='documentation')
    sys.source( test.file, environment(), keep.source=TRUE)
    expect_true(exists("example_function2"))

    expect_warning( docs <- extract_documentation(example_function2)
                  , class="documentation-warning-orphan_comment"
                  )

    expect_is(docs, 'function-Documentation')
    expect_identical(docs@arguments$x@description, "The x argument description.")
    expect_identical(docs@arguments$y@description, "The y argument description takes 2 lines.")
    expect_identical(docs@name, as.name("example_function2"))
}
extract_documentation.standardGeneric <-
function( object
        , ...
        , srcfile = NULL
        , pd = NULL
        , roxy.block = NULL
        , markdown=FALSE
        , name = substitute(object)
        ){
    default.method <- attr(object, 'default')
    if (is.null(pd)){
        pd <- get_parse_data(default.method)
        pd <- pd_get_family(id=attr(pd, 'id'), pd=pd)
    } else if(!inherits(pd, 'parse-data'))
        doc_error('Bad parse data.', type = "bad_pd")

    if(is.null(srcfile)){
        srcref <- utils::getSrcref(default.method)
        if (is.null(srcref)) doc_no_src(name)
        srcfile <- utils::getSrcFilename(srcref, full.names = TRUE, unique = TRUE)
    }

    if (is.null(roxy.block))
        roxy.block <- .get_roxy_block(object, srcfile=srcfile, ...)

    docs <- .construct_documentation.function(object, roxy.block, pd, name=name)
    if (.is_undefined(docs@name)) docs@name <- as.name(name)
    if (length(object@valueClass) & is.na(docs@value))
        docs@value <- FormattedText(
            ._( "Methods are restricted to returning an object of class %s."
              , comma_list(object@valueClass, sep2 = ' or ', sep.last = ", or")))

    return(docs)
}
if (FALSE) {#@testing with example_generic
    env <- new.env()
    env$.packageName <- "documentation-testing-environment"

    test.file <- system.file("examples", "standardGeneric.R", package = "documentation")
    sys.source( test.file, envir = env , keep.source = TRUE)
    docs <- with(env, extract_documentation(example_generic))

    expect_identical(docs@name, as.name("example_generic"))
    expect_identical(docs@title, "Example Generic Function")
    expect_identical(docs@description
                    , "This is an example of an S4 standardGeneric " %\%
                      "documentation.  This is to be used for testing" %\%
                      "of the functions in the documentation package."  %>%
                        FormattedText())
    expect_identical(docs@value, FormattedText("Methods are restricted to returning an object of class logical."))
}
