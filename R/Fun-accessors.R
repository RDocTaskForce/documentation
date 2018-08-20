#' @include Classes.R
#' @include Fun-documentation.R


# nocov start
.define_generic_doc_accessor <-
function(name, valueClass="character", where = topenv()){
    if (length(name)>1L) return(lapply(name, .define_generic_doc_accessor))

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
# nocov end

local({
    documentation.slots <- 
        c( getSlots(getClass('Documentation'))
         , getSlots(getClass('BaseDocumentation'))
         , getSlots(getClass('function-Documentation'))
         , getSlots(getClass('option-Documentation'))
         )
    documentation.slots <- 
        documentation.slots[unique(names(documentation.slots))]
    for (i in seq_along(documentation.slots)){
        slot.name <- names(documentation.slots)[[i]]
        slot.class <- documentation.slots[[i]]
        .define_generic_doc_accessor(slot.name, slot.class)
    }
})
# .define_generic_doc_accessor(slotNames(getClass('Documentation')))

.define_generic_doc_accessor('name', 'character')
.define_generic_doc_accessor('details', 'FormattedText')
.define_generic_doc_accessor('usage', 'call')

if(FALSE){#@testing generic accessors
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


