# inst/examples/standardGeneric.R

#' Example Generic Function
#' 
#' This is an example of an S4 standardGeneric 
#' documentation.  This is to be used for testing
#' of the functions in the documentation package.
#' 
#' @keywords documentation misc
setGeneric( "example_generic"
          , valueClass = "logical"
          , function(x, ...){return(NA)}
          )

