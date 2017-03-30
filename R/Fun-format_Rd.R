#' @include options.R 

setGeneric('format_Rd', 
    function(object, ...){
        #TODO Program default format_Rd
        if(isS4(object)){
            slot.names <- slotNames(object)
            slots      <- lapply(slot.names, slot, object=object)
            formatted  <- sapply(slots, format_Rd)
            interleave( sprintf("@%s:", names)
                      , formatted
                      )
        } else if(is.list(object)) {
            interleave( sprintf("$%s:", names(object))
                      , lapply(object, format_Rd)
                      )
        } else {
            format(object, ...)
        }
    })

#~ defaults$format_Rd <- new.env(hash=TRUE, parent=defaults)

set_option_documentation( "documentation::format_Rd::indent"
   , description = "Determines if code should be indented when formatted.  Should default to FALSE when unset."
   )


#~ defaults$format_Rd$indent.with <- '    '

set_option_documentation( "documentation::format_Rd::indent.with"
   , description = "Determines what to indent with, when getOption('documentation::format_md::indent') is TRUE."
   )


#~ defaults$format_md$collapse.lines <- FALSE

set_option_documentation("documentation::format_Rd::collapse.lines"
   , description = "should documentation functions return a single " %\%
                   "string (TRUE) or a array of strings (FALSE) " %\%
                   "representing the lines of documentation."
   )

#~ defaults$format_md$collapse.with <- '\n'
set_option_documentation("documentation::format_Rd::collapse.with"
   , description = "when \\code{getOption(documentation::format_md::collapse.lines)}" %\%
                   "is \\code{TRUE} what the lines should be separated with."
   )




