#' @include setup.R
#' @include utils.R
#' @include Class-Documentation.R
#' @include Class-function-Documentation.R
#' @include Class-option-Documentation.R
#' @include Class-data-Documentation.R
#' @include Class-S4-Documentation.R


### Class: Shared-Documentation(VIRTUAL) #####
#' Documentation Shared by multiple objects
#'
#' A wrapper class for sharing documentation among multiple objects.
#'
#' @export
setRefClass( "Shared-Documentation", contains=c('VIRTUAL')
           , fields = list( docs = 'Documentation'
                          , exports = "ReferenceSet<Export>"
                          )
           , validity=function(object){
               validate_that( is(object$docs, "Documentation")
                            , rlang::is_dictionaryish(object@export)
                            , all(names(object@export) %in% doc_get_aliases(object))
                            )
           }
           , methods = list(
                .add = function(what, value, unique=TRUE, sorted=FALSE){
                    new <- c(slot(docs, what), value)
                    if (unique) new <- base::unique(new)
                    if (sorted) new <- sort(new)
                    slot(docs, what) <<- new
                    invisible(.self)
                },
                add = function(what, value, ...){
                    if (exists( . <- "add_" %<<<% what
                              , .refClassDef@refMethods
                              )) {
                        adder <- eval(call('$', as.name('.self'), as.name(.)))
                        adder(value, ...)
                    } else {
                        .add(what, value, ...)
                    }
                    return(invisible(.self))
                },
                add_named = function(what, name, value, ...){
                    # browser()
                    if (exists( . <- "add_" %<<<% what
                              , .refClassDef@refMethods
                              )){
                        adder <- eval(call('$', as.name('.self'), as.name(.)))
                        return(adder(name=name, value, ...))
                    } else {
                        slot(docs, what) <<- c(slot(docs, what), s(value, names=name))
                    }
                },
                add_alias = function(alias)add_aliases(alias),
                add_aliases = function(aliases){
                    assert_that( is.character(aliases)
                               , !any(is.na(aliases))
                               , nchar(aliases) > 0L
                               )
                    .add('aliases', aliases, TRUE, TRUE)
                },
                add_argument  = function(arg)add_arguments(arg),
                add_arguments = function(args){
                    if (!is(args, 'ArgumentList')) {
                        if (canCoerce(args, 'ArgumentList'))
                            args <- as(args, 'ArgumentList')
                        else
                            doc_error_bad_argument( args, 'ArgumentList'
                                                  , scope = c( 'documentation'
                                                             , 'Shared-Documentation'
                                                             , 'add_arguments'
                                                             ))
                    }

                    current.args <- as.character(purrr::map(docs@arguments, doc_get_name))
                    other.args   <- as.character(purrr::map(args, doc_get_name))
                    if (length(. <- intersect(current.args, other.args))){
                        for (arg in .)
                            if (!identical(args[[arg]], docs@arguments[[arg]]))
                                doc_error(._("Shared documentation for %s already contains an argument for %s."
                                            , sQuote(doc_get_name(docs)), sQuote(arg)))
                    }
                    docs@arguments <<- unique(c(docs@arguments, args))
                    invisible(.self)
                },
                add_author = function(value){
                    if (doc_has_author(docs)){
                        if (!all(. <- value %in% doc_get_author(docs))){
                            doc_warning(._("Shared documentation already contains an author."))
                            doc_author(docs) <<- c(doc_get_author(docs), value[!.])
                        }
                    } else {
                        doc_author(docs) <<- value
                    }
                    invisible(.self)
                },
                add_concepts = function(value){.add('concepts', value, unique=TRUE, sorted=TRUE)},
                add_concept  = function(value){.add('concepts', value, unique=TRUE, sorted=TRUE)},
                add_examples = function(value){.add('examples', value, unique=FALSE, sorted=FALSE)},
                add_example  = function(value){.add('examples', value, unique=FALSE, sorted=FALSE); },
                add_export = function( value){
                    assert_that(is(value, "Export"))
                    exports$add(value)
                    invisible(.self)
                },
                add_keyword = function(value){.add('keywords', as(value, 'Documentation-Keyword'), unique=TRUE, sorted=TRUE)},
                add_keywords = function(value){.add('keywords', as(value, 'Documentation-Keyword'), unique=TRUE, sorted=TRUE)},
                add_description = function(value, name = NULL, ...){
                    if (!is(value, 'FormattedText'))
                        value <- FT(value)
                    old <- doc_get_description(docs)
                    if (is.null(name)) {
                        doc_description(docs) <<-
                            if (length(old)==0) value else c(old, value)
                    } else {
                        new <- c( old, subsection(name, value))
                        doc_description(docs) <<- new

                    }
                    invisible(.self)
                },
                add_details = function(value, name = NULL, ...){
                    if (is.null(name)) {
                        doc_details(docs) <<-
                            c( doc_details(docs), value)
                    } else {
                        doc_details(docs) <<-
                            c( doc_details(docs), subsection(name, value))
                    }
                    invisible(.self)
                },
                add_name = function(value){
                    if (doc_has_name(docs))
                        doc_error("Documentation name is already set.")
                    else
                        doc_name(docs) <<- value
                },
                add_title = function(title){
                    if (doc_has_title(docs)){
                        msg <- ._("Shared documentation already contains a title.")
                        if (doc_get_title(docs) != title)
                            doc_error(msg)
                        else
                            doc_warning(msg)
                    } else {
                        assert_that(rlang::is_string(title))
                        docs@title <<- title
                    }
                    invisible(.self)
                },
                merge = function(new){
                    browser()
                    assert_that(is(new, ))



                }
                )
           )
if(FALSE){#@testing Shared-Documentation
    doc <- shared(function_documentation(name='test-doc'))
    doc2 <- shared(function_documentation())
    expect_is_exactly(doc, 'Shared-function-Documentation')
    expect_identical(doc_get_name(doc), 'test-doc')

    # aliases
    doc$add('aliases', 'alien')
    expect_identical(doc_get_aliases(doc), c('test-doc', 'alien'))
    expect_identical(doc$docs@aliases, 'alien')
    expect_identical(doc$docs@aliases, 'alien')

    # arguments
    args <- AL(arg(a, 'first'), arg(b, 'second'))
    doc$add('arguments', args)


    # author
    expect_identical(doc$add('author', person('Billy Bo Bob', 'Brain')), doc)
    expect_silent(doc$add('author', person('Billy Bo Bob', 'Brain')))
    expect_warning(doc$add('author', person('Yacko', 'Warner')))

    # concepts
    expect_identical(doc$add('concepts', 'testing'), doc)
    expect_identical(doc_get_concepts(doc), 'testing')
    expect_identical(doc$add('concepts', 'testing'), doc)
    expect_identical(doc_get_concepts(doc), 'testing')
    expect_identical(doc$add('concepts', 'expansion'), doc)
    expect_identical(doc_get_concepts(doc), c('expansion', 'testing'))

    # description
    expect_identical(doc_get_description(doc), FT())
    new <- stringi::stri_rand_lipsum(1)
    expect_identical(doc$add('description', new), doc)
    expect_identical(doc_get_description(doc), FT(new))
    part2 <- stringi::stri_rand_lipsum(1)
    expect_identical(doc$add('description', part2, name='Part 2'), doc)

    # examples
    expect_identical(doc_get_examples(doc), new('Documentation-Examples'))
    expect_identical(doc$add('example', "# Just an example" %\%
                                        "`test-doc`()" %\%
                                        "alien()"), doc)

    expect_length(doc_get_examples(doc), 1L)
    exp <- as(expression( `test-doc`()
                        , alien()
                        ), 'example')
    expect_identical( no_src(doc_get_examples(doc)[[1]]), exp)

    # export
    expect_identical(doc$add('export', export('alien')), doc)
    expect_equal(format(doc_get_export(doc)), 'export(alien)')
    expect_error(doc$add('export', export_pattern('alien')))

    # keywords
    expect_identical(doc$add('keyword', 'methods'), doc)
    expect_length(doc_get_keywords(doc), 1L)

    # name
    expect_error(doc$add('name', 'monster'))
    expect_identical(doc_get_name(doc), 'test-doc')
    expect_identical(doc2$add('name', 'Yacko'), doc2)
    expect_identical(doc_get_name(doc2), 'Yacko')
    expect_error(doc2$add('name', 'Wacko'))

    # references
    # TODO

    # sections
    # TODO

    # seealso
    # TODO

    # title
    expect_identical(doc$add('title', "Test Documentation"), doc)
    expect_identical(doc_get_title(doc), "Test Documentation")
    expect_error(doc$add('title', "Not a title"))

    # usage
    new.use <- as(substitute(alien(planet, ship, weapons)), 'usage')
    expect_identical(doc$add('usage', new.use), doc)
    expect_identical( doc_get_usage(doc)
                    , UsageList( list( usage( expression(`test-doc`()))
                                     , new.use))
                    )

    # value
    # TODO
}

### Condition functions #####
doc_error_shared_replacement <-
function(...){doc_error( ._("Replacement of shared documentation objects not allowed.")
                       , type = 'shared-replacement'
                       , ...)}

doc_warning_shared_existing <-
function(name, slot, ...){doc_warning( ._("Documentation of %s already has a %s."
                                         , sQuote(name), slot)
                                     , type="shared-existing", ...)}

### Documentation to Shared-Documentation #####
shared <- function(doc){
    assert_that(is(doc, 'Documentation'))

    as(doc, 'Shared-Documentation')
}
if(FALSE){#@testing
    doc <- function_documentation()
    expect_is_exactly(doc, 'function-Documentation')

    dcs <- shared(doc)
    expect_is_exactly(dcs, 'Shared-function-Documentation')
}
if(FALSE){#@testing
    doc <- data_documentation()
    expect_is_exactly(doc, 'data-Documentation')

    dcs <- shared(doc)
    expect_is_exactly(dcs, 'Shared-data-Documentation')
}

### Shared-Documentation is Documentation #####
setIs('Shared-Documentation', 'Documentation'
     , coerce = function(from)from$docs
     , replace = function(object, value)
         doc_error(._("Replacement of shared documentation objects not allowed."))
     )
if(FALSE){#@testing Class Shared-Documenation
    hw <- function(){print("hello world")}

    docs <- function_documentation("hw", title = "Hello World")
    shared.docs <- shared(docs)
    expect_is(shared.docs, 'Documentation')
    expect_is(shared.docs, 'Shared-Documentation')
}

### Dynamically Created Classes ======
#' Dynamically create shared documentation types.
#'
#' Each type of documentation (data, function, etcetera.)
#' can be turned into a shared documentation type through
#' this function.
.create_shared_doc_type <-
function( doc.class  #< name of the documentation class
        , shared.class = paste0('Shared-', doc.class) #< name of the new shared type.
        , fields = character()
        , ...
        , where = topenv()
        ){
    init <- no_src(eval(substitute(function(..., doc){
        if (!missing(doc)){
            assert_that( ...length() == 0L
                       , is(doc, doc.class)
                       )
        } else {
            doc <- new(doc.class, ...)
        }
        docs <<- doc
    }, list(doc.class=doc.class)), where))
    formals(init)$docs = substitute(new(doc.class, ...), list(doc.class=doc.class))

    generator <-
    setRefClass( Class = shared.class
               , contains = 'Shared-Documentation'
               , fields = c(docs=doc.class, fields)
               , where = where
               , methods = list(initialize = init)
               , ...
               )

    as.method <- no_src(eval(substitute(function(from){
        new.doc <- new(shared.class, doc=from)
        if (.hasSlot(from, 'usage')){
            if (doc_has_name(from) && is(from@usage, 'waiver'))
                doc_usage(new.doc) <- doc_get_usage(from)
        }
        if (.hasSlot(from, 'export')){
            if (is.logical(doc_get_export(from)) && isTRUE(doc_get_export(from)))
                new.doc$add_export(export(doc_get_name(from)))
        }
        return(new.doc)
    }, list(shared.class=shared.class)), where))
    setAs(doc.class, shared.class, as.method, where=where)
    setAs(doc.class, 'Shared-Documentation', as.method, where=where)
    setIs( shared.class, doc.class
         , coerce = function(object)object$docs
         , where = where
         , replace = function(object, value) doc_error_shared_replacement()
         )
    invisible(generator)
}
shared_data_documentation <- .create_shared_doc_type( 'data-Documentation')
shared_function_documentation <- .create_shared_doc_type('function-Documentation')
# shared_option_documentation <- .create_shared_doc_type('option-Documentation')
if(FALSE){#@testing
    sfd <- getClass('Shared-function-Documentation')
    expect_identical(sfd@fieldClasses$docs, 'function-Documentation')

    sdd <- getClass('Shared-data-Documentation')
    expect_identical(sdd@fieldClasses$docs, 'data-Documentation')

    doc <- shared_function_documentation( 'testing-shared'
                                        , title = 'Testing Shared Function Documentation'
                                        )
    expect_is(doc, 'Shared-function-Documentation')
    expect_is(doc, 'Shared-Documentation')
    expect_is(doc, 'function-Documentation')

    d2 <- function_documentation('test-doc')
    doc_usage(d2) <- doc_get_usage(d2)
    expect_is_exactly(d2, 'function-Documentation')
    expect_false(is(d2, 'Shared-Documentation'))

    d3 <- as(d2, 'Shared-function-Documentation')
    expect_is_exactly(d3, 'Shared-function-Documentation')
    expect_equal(d3$docs, d2)

    d4 <- as(d2, 'Shared-Documentation')
    expect_is_exactly(d4, 'Shared-function-Documentation')
    expect_identical(d4$docs, d2)
}


### Functions #####

doc_combine <- function (...) UseMethod("doc_combine")

`doc_combine.function-Documentation` <- function(...){
    docs <- list(...)
    assert_that(all_inherit(docs, 'function-Documentation', '`...`'))

    if (length(docs) == 0L)
        doc_error("no documentation to combine")
    if (length(docs) == 1L)
        return(docs[[1]])

    names(docs) <- purrr::map_chr(docs, doc_get_name)

    shared <- as(docs[[1]], 'Shared-Documentation')
}

### Generic: doc_augment #####
setGeneric('doc_augment', function(shared, doc) standardGeneric("doc_augment"))
### Method: doc_augment,function-Documentation #####
# setMethod( 'doc_augment'
#          , c('Shared-function-Documentation', 'function-Documentation')
#          , function(shared, doc){
#
#
#
# })
### Method: doc_augment,S3method-Documentation #####
setMethod( 'doc_augment'
         , c('Shared-function-Documentation', 'S3method-Documentation')
         , function(shared, doc){
    name <- doc_get_name(doc)

    # name
    if (.is_undefined(shared$docs@name))
        doc_name(shared) <- doc_get_name(doc)
    # title
    if (length(. <- doc_get_title(doc)) > 0L){
        if (length(doc_get_title(shared))>0L)
            doc_warning_shared_existing(doc_get_name(shared), 'title')
        else
            doc_title(shared) <- .
    }
    # description & details
    if (doc_has_description(doc) && doc_has_details(doc)){
        shared$add_named('description', doc_get_name(doc), doc_get_description(doc))
        shared$add_named('details'    , doc_get_name(doc), doc_get_details(doc))
    } else if(doc_has_description(doc)) {
        shared$add_named('details', doc_get_name(doc), doc_get_description(doc))
    } else if(docs_has_details(doc)) {
        shared$add_named('details', doc_get_name(doc), doc_get_details(doc))
    }

    # export
    shared$add('export', )

    use.named <- c( "value", 'export')

    others <- c( "aliases", "arguments", "author", "concepts"
               , "examples", "keywords", "references"
               , "seealso", "sections", 'usage')

    for (name in others)
        shared$add(name, doc_get(doc, name))
    return(invisible(shared))
})
if(FALSE){

    shared <- shared(
        function_documentation( 'html_to_Rd'
                              , title  = 'Convert html shiny.tag objects to Rd.'
                              , value  = 'a cannonical Rd object.'
                              , author = person('Andrew', 'Redd')
                              , export = TRUE
                              ))

    shared <- shared_function_documentation()
    doc <- S3method_documentation( "html_to_Rd", 'a'
                                 , description =
                                     "Convert html link to Rd link."
                                 )


}




if(FALSE){
    base <- function_documentation( name = 'base'
                                  , title = 'Testing Combined Documentation'
                                  , arguments = AL( arg(x, 'An object')
                                                  , arg_('...', 'other arguments')
                                                  )
                                  , description = FT('Describes the general function')
                                  , details = FT("base is a generic function...")
                                  )

    d1 <- S3method_documentation( 'base', 'default'
                                ,  arguments = AL( arg(arg1, 'argument only present in 1 version')
                                                 , arg_('...', 'other arguments') )
                                )

    d2 <- S3method_documentation( 'base', 'class1'
                                , arguments = AL( arg(arg2, 'argument only for class1')
                                                , arg_('...', 'other arguments'))
                                )
    docs <- list(base, d1, d2)
}




