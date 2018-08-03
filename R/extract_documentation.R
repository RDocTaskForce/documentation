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


.construct_documentation_function <-
function( roxy.block            #< roxy_block for object
        , pd                    #< parse.data for object
        , doc.class = 'function-Documentation'
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
                                , docs@name, arg.name))
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

    docs <- .construct_documentation_function(roxy.block, pd)
    expect_is(docs, 'function-Documentation')


}
if(FALSE){#@testing



    test.file <- system.file("examples", "example_character.R", package='documentation')
    sys.source( test.file, keep.source=TRUE)
    object <- example_character

    expect_error(.get_roxy_block(example_character), class='documentation-error-no_src')
    roxy.block <- .get_roxy_block(example_character, srcfile=test.file)
    loc <- attr(roxy.block, 'location')


    pd <- get_parse_data(parse(file=test.file, keep.source=TRUE))
    pd_identify(pd, object)
    pd_identify(pd, srcref(srcfile(test.file), loc))


    pd[ pd$line1 == loc[1]
      & pd$col1  == loc[5]
      & pd$line2 == loc[3]
      & pd$col2  == loc[6]
      , 'id']

    pd2 <- srcref(srcfile(test.file), attr(block, 'location')) %>% get_parse_data()
}


extract_documentation <-
function( object   #< Object for which to extract documentation.
        , ...      #< passed on to methods.
        , name = substitute(object) #< explicit naming for indirect calling.
        )UseMethod('extract_documentation')

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
    return(.construct_documentation_function(roxy.block, pd))

    docs <- if (has.roxy){
        as(roxy.block, 'function-Documentation')
    } else new('function-Documentation')
    if (length(rel.comments)) {
        associated <- pd_get_relative_comment_associated_ids(rel.comments, pd)
        for (a in unique(associated)){
            if (pd_is_function_arg(a, pd, .check=FALSE)){
                arg.name <- pd_text(a, pd)
                if (arg.name %in% names(docs@arguments))
                    doc_error(._( "documentation for %s already contains" %<<%
                                  "documentation for argument %s"
                                , docs@name, arg.name))
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

    roxy.block <- .get_roxy_block(object, srcfile=srcfile, ...)

    docs <- .construct_documentation_function(roxy.block, pd)
    if (.is_undefined(docs@name)) docs@name <- as.name(name)
    if (length(object@valueClass) & is.na(docs@value))
        docs@value <- FormattedText(
            ._( "Methods are restricted to returning an object of class %s."
              , comma_list(object@valueClass, sep2 = ' or ', sep.last = ", or")))

    return(docs)
}
if (FALSE) {#@testing with example_generic
    sys.source( system.file("examples", "standardGeneric.R", package = "documentation")
              , environment() , keep.source = TRUE)
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
