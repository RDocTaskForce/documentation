#' @include Classes.R
#' @include Fun-documentation.R


.define_generic_doc_accessor <-
function( name
        , valueClass="character"  #< Type of object methods are expected to return
        , where = topenv()        #< Namespace to place definition of generic in.
        , setter=TRUE             #< Define `doc_*<-` function
        , has=TRUE                #< Define `doc_has_*` function
        , ...                     #< Passes on to setGeneric
        ){
    if (length(name)>1L) return(lapply(name, .define_generic_doc_accessor
                                      , where=where, setter=setter))

    fun.name <- "doc_get_"  %<<<% name
    def <- substitute(function(doc){
        if (hasMethod(fun.name, class(doc)))
            return(standardGeneric(fun.name))
        else if (!.hasSlot(doc, name))
            pkgcond::pkg_error(._("Class '%s' does not have a slot named '%s'"
                                 , class(doc), name)
                              , type = 'invalid-slot'
                              , class = class(doc), slot=name)
        else
            return(as(slot(doc, name), valueClass))
    }, env = list(name = name, fun.name = fun.name, valueClass=valueClass ))
    fun = eval(def, envir = where)
    setGeneric( fun.name, valueClass=valueClass, def=fun, where = where
              , package = getPackageName(where)
              , ...)
    if (.document.generated) {
        docs <-
        documentation(where[[fun.name]]) <- #
            shared_function_documentation( name = fun.name
                                         , title = "Documentation accessor for" %<<% name
                                         , description = FT("This is an automatically generated function for accessing the" %<<%
                                        name %<<% "of a documentation object." %<<%
                                        "Specific methods may override the default behavior, " %<<%
                                        "especially when the" %<<% name %<<%
                                        "is expected to conform to a standard" %<<%
                                        "or is generated from other known information.")
                                  , arguments = AL(doc = arg( doc, "documentation object"
                                                            , constraints = list(~is(., "Documentation"))
                                                            ))
                                  # , sections = section( "functions"
                                  #                     , FT_Rd(Rd_item(fun.name, "Get the documentation " %<<% name )))
                                  )#)
    }
    if (setter){
        setter.name <- "doc_" %<<<% name %<<<% '<-'
        setter.def <- substitute(function(doc, value){
            if (hasMethod(setter.name, class(doc)))
                return(standardGeneric(setter.name))
            if (!.hasSlot(doc, name)){
                pkgcond::pkg_error(._("Class '%s' does not have a '%s' which can be set."
                                     , class(doc), name)
                                  , type = 'invalid-slot'
                                  , class = class(doc), slot=name)
            }
            doc@name <- as(value, getElement(getSlots(getClass(class(doc))), name))
            return(invisible(doc))
        }, env = list(name = name, setter.name = setter.name, valueClass=valueClass))
        setter <- no_src(eval(setter.def, envir=where))
        setGeneric( setter.name, def = setter, where=where
                  , package = getPackageName(where)
                  , ...)
        if (.document.generated){
            # docs$add_argument(arg(value, "Replacement value."))
            # docs$add_alias(setter.name)
            # documentation(setter) <- docs
        }
    }
    if (has){
        has.name <- 'doc_has_' %<<<% name
        has.def <- substitute(function(doc){
            length(get(doc)) > 0L
        }, list(get = as.name(fun.name)))
        has <- no_src(eval(has.def, envir=where))
        setGeneric( has.name, def = has, where=where
                  , package = getPackageName(where)
                  , ...)
    }
    invisible(TRUE)
}
if(FALSE){#@testing
    if (isNamespaceLoaded("my-test-package"))
        unloadNamespace('my-test-package')

    env <- pkgcond::suppress_warnings(pattern = "replacing previous import",{
        testextra::new_pkg_environment('my-test-package', import = c('methods', 'documentation')
                                      , register = TRUE)
    })
    env$env <- env
    # tryCatch({
    .define_generic_doc_accessor('test_slot', where = env)

    expect_true(exists('doc_get_test_slot', where = env))
    getter <- get('doc_get_test_slot', env)
    expect_is(getter, "nonstandardGenericFunction")

    expect_true(exists('doc_test_slot<-', where = env))
    setter <- get('doc_test_slot<-', env)
    expect_is(setter, "nonstandardGenericFunction")

    expect_true(exists('doc_has_test_slot', where = env))
    has <- get('doc_has_test_slot', env)
    expect_is(has, "standardGeneric")

    val <- .define_generic_doc_accessor(c('test1', 'test2'), where=env)
    expect_identical(val, list(TRUE, TRUE))
    expect_true(exists('doc_get_test1', where = env))
    expect_true(exists('doc_get_test2', where = env))

    myDoc <- setClass( 'myDoc', contains = 'Documentation'
                     , list(test_slot = 'name', test1='character')
                     , where = env
                     )
    setMethod('doc_get_test2', 'myDoc', function(doc)"prepare to die."
             , where = env )
    setMethod('doc_test_slot<-', 'myDoc'
             , function(doc, value){
        value <- as.name(value)
        doc@test_slot <- value
        return(invisible(doc))
    }, where=env)

    env$doc <- myDoc( test_slot = as.name('Inigo Montoya')
                    , test1 = "you kill my father"
                    )

    expect_is(env$doc, 'myDoc')
    expect_true(with(env, doc_has_test_slot(doc)))
    expect_true(with(env, doc_has_test1(doc)))
    expect_true(with(env, doc_has_test2(doc)))

    expect_identical(with(env, doc_get_test_slot(doc)), "Inigo Montoya")
    expect_identical(with(env, doc_get_test1(doc)), "you kill my father")
    expect_identical(with(env, doc_get_test2(doc)), "prepare to die.")

    with(env, doc_test_slot(doc) <- "Thanos")
    expect_identical(with(env, doc_get_test_slot(doc)), "Thanos")
    with(env, doc_test1(doc) <- "I kill everyone.")
    expect_identical(with(env, doc_get_test1(doc)), "I kill everyone.")
    expect_error(with(env, doc_test2(doc) <- "I'll kill you to."))

    .define_generic_doc_accessor(c('bad_slot'), where=env)
    # expect_error(
        cond <- catch_condition(with(env, doc_get_bad_slot(doc)))
                # , class='error-invalid-slot' )
    # }, finally =
        testextra::unregister_namespace(env)
    # )
}


### Non-specific accessors #####
all.documentation.slots <-
    c( getSlots(getClass('Documentation'))
     , getSlots(getClass('BaseDocumentation'))
     , getSlots(getClass('function-Documentation'))
     , getSlots(getClass('option-Documentation'))
     )
all.documentation.slots <-
    all.documentation.slots[unique(names(all.documentation.slots))]
for (i in seq_along(all.documentation.slots)){
    slot.name <- names(all.documentation.slots)[[i]]
    slot.class <- all.documentation.slots[[i]]
    .define_generic_doc_accessor(slot.name, slot.class)
}

### Name accessors #####
.define_generic_doc_accessor('name', 'character', has=FALSE)
setGeneric( "doc_has_name"
          , function(doc){
              !.is_undefined(doc@name)
          })
if(FALSE){#@testing
    doc <- function_documentation()
    expect_false(doc_has_name(doc))
    doc_name(doc) <- 'test'
    expect_true(doc_has_name(doc))
    expect_identical(doc_get_name(doc), 'test')
}

if(FALSE){#@testing generic accessors
    if (.document.generated){
        expect_is(doc <- documentation(doc_get_name), 'function-Documentation')
        expect_equal(doc@title, "Documentation accessor for name")
        expect_equal( doc@description
                    , FormattedText("This is an automatically generated function for accessing the" %<<%
                                    "name of a documentation object." %<<%
                                    "Specific methods may override the default behavior, " %<<%
                                    "especially when the name" %<<%
                                    "is expected to conform to a standard" %<<%
                                    "or is generated from other known information."))
        expect_equal(names(doc@arguments), "doc")

        expect_is(doc <- documentation(doc_get_title), 'function-Documentation')
        expect_equal(doc@title, "Documentation accessor for title")
        expect_equal( doc@description
                    , FormattedText("This is an automatically generated function for accessing the" %<<%
                                    "title of a documentation object." %<<%
                                    "Specific methods may override the default behavior, " %<<%
                                    "especially when the title" %<<%
                                    "is expected to conform to a standard" %<<%
                                    "or is generated from other known information."))
        expect_equal(names(doc@arguments), "doc")
    }
}
if(FALSE){#@testing doc_has_*
    doc <- function_documentation("test", title="Test me!")

    expect_true(doc_has_name(doc))
    expect_true(doc_has_title(doc))
    expect_true(doc_has_export(doc))
    expect_false(doc_has_description(doc))
    expect_false(doc_has_details(doc))
}


### Details accessors #####
.define_generic_doc_accessor('details', 'FormattedText')
setMethod('doc_get_details', 'Documentation', function(doc){
    doc@sections[['details']]
})
setMethod('doc_details<-', 'Documentation', function(doc, value){
    doc@sections[['details']] <- value
    return(doc)
})
if(FALSE){#@testing
    doc <- function_documentation(name='test-doc')
    det <- asection(list(FT(stringi::stri_rand_lipsum(3))))

    expect_null(doc_get_details(doc))
    doc_details(doc) <- det
    expect_identical(doc_get_details(doc), det)
}


# Very Generic Accessors ==================
doc_has <- function(doc, name){
    fun <- try(match.fun('doc_has_' %<<<% name), silent = TRUE)
    if (is.function(fun))
        return(fun(doc))
    name %in% slotNames(doc) &&
        (length(slot(doc, name)) > 0L)
}
doc_get<- function(doc, name){
    fun <- match.fun('doc_get_' %<<<% name)
    return(fun(doc))
}
if(FALSE){#@testing doc_has & doc_get
    doc <- function_documentation( name = "Normal"
                                 , title = "The Normal Distribution"
                                 , aliases = c('rnorm', 'dnorm', 'pnorm', 'qnorm')
                                 )
    expect_true(doc_has(doc, 'name'))
    expect_true(doc_has(doc, 'title'))
    expect_true(doc_has(doc, 'aliases'))

    expect_identical(doc_get(doc, 'name'), "Normal")
    expect_identical(doc_get(doc, 'title'), "The Normal Distribution")
    expect_identical(doc_get(doc, 'aliases'), c('Normal', 'dnorm', 'pnorm', 'qnorm', 'rnorm'))

    doc <- S3method_documentation('test', 'me')

    expect_true(doc_has(doc, 'generic'))
    expect_true(doc_has(doc, 'signature'))
}

