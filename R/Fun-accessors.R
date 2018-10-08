#' @include Classes.R
#' @include Fun-documentation.R


# nocov start
.define_generic_doc_accessor <-
function( name, valueClass="character", where = topenv()
        , setter=TRUE #< Define `doc_*<-` function
        , has=TRUE   #< Define `doc_has_*` function
        ){
    if (length(name)>1L) return(lapply(name, .define_generic_doc_accessor
                                      , where=where, setter=setter))

    fun.name <- "doc_get_"  %<<<% name
    def <- substitute(function(doc){
        if (hasMethod(fun.name, class(doc)))
            return(standardGeneric(fun.name))
        else if (!.hasSlot(doc, name))
            doc_error(._("Class '%s' does not have a slot named '%s'"
                        , class(doc), name)
                     , type = 'invalid-slot'
                     , class = class(doc), slot=name)
        else
            return(as(slot(doc, name), valueClass))
    }, env = list(name = name, fun.name = fun.name, valueClass=valueClass ))
    fun = eval(def, envir = where)
    setGeneric(fun.name, valueClass=valueClass, def=fun, where = where)
    if (.document.generated) {
        documentation(where[[fun.name]]) <-
            function_documentation( name = fun.name
                                  , title = "Documentation accessor for" %<<% name
                                  , description = FT("This is an automatically generated function for accessing the" %<<%
                                        name %<<% "of a documentation object." %<<%
                                        "Specific methods may override the default behavior, " %<<%
                                        "especially when the" %<<% name %<<%
                                        "is expected to conform to a standard" %<<%
                                        "or is generated form other known information.")
                                  )
    }
    if (setter){
        setter.name <- "doc_" %<<<% name %<<<% '<-'
        setter.def <- substitute(function(doc, value){
            if (hasMethod(setter.name, class(doc)))
                return(standardGeneric(setter.name))
            if (.hasSlot(doc, name)){
                doc@name <- as(value, getElement(getSlots(getClass(class(doc))), name))
                return(doc)
            }
            doc_error(._("Class '%s' does not have a '%s' which can be set."
                        , class(doc), name)
                     , type = 'invalid-slot'
                     , class = class(doc), slot=name)
        }, env = list(name = name, setter.name = setter.name, valueClass=valueClass))
        setter <- no_src(eval(setter.def, envir=where))
        setGeneric( setter.name, def = setter, where=where)
    }
    if (has){
        has.name <- 'doc_has_' %<<<% name
        has.def <- substitute(function(doc){
            length(get(doc)) > 0L
        }, list(get = as.name(fun.name)))
        has <- no_src(eval(has.def, envir=where))
        setGeneric( has.name, def = has, where=where)
    }
    invisible(TRUE)
}
# nocov end


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

.define_generic_doc_accessor('name', 'character')
.define_generic_doc_accessor('details', 'FormattedText')

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
                                    "or is generated form other known information."))

        expect_is(doc <- documentation(doc_get_title), 'function-Documentation')
        expect_equal(doc@title, "Documentation accessor for title")
        expect_equal( doc@description
                    , FormattedText("This is an automatically generated function for accessing the" %<<%
                                    "title of a documentation object." %<<%
                                    "Specific methods may override the default behavior, " %<<%
                                    "especially when the title" %<<%
                                    "is expected to conform to a standard" %<<%
                                    "or is generated form other known information."))
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


setMethod('doc_get_details', 'Documentation', function(doc)doc@sections[['details']])
setMethod('doc_details<-', 'Documentation', function(doc, value){
    doc@sections[['details']] <- value
    return(doc)
})
if(FALSE){#@testing
    doc <- function_documentation(name='test-doc')
    det <- FT(stringi::stri_rand_lipsum(3))

    expect_null(doc_get_details(doc))
    doc_details(doc) <- det
    expect_identical(doc_get_details(doc), det)
}

