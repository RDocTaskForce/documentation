
#' Sharing Documentation
#' 
#' A wrapper class for sharing documentation among multiple objects.
#' 
#' @export
shared <- 
setRefClass( "Shared-Documentation", contains='Documentation'
           , fields = list(docs='ANY')
           , validity=function(object){
               validate_that(is(object$docs, "Documentation"))
           })
setAs('Documentation', 'Shared-Documentation', function(from)shared(docs=from))
# setAs('Shared-Documentation', 'Documentation', function(from)from$docs)
setIs('Shared-Documentation', 'Documentation'
     , coerce = function(from)from$docs
     , replace = function(object, value)
         doc_error(._("Replacement of shared documentation objects not allowed."))
     )
if(FALSE){#@testing Class Shared-Documenation
    hw <- function(){print("hello world")}
    
    docs <- function_documentation("hw", title = "Hello World")
    shared.docs <- shared(docs=docs)
    expect_is(shared.docs, 'Documentation')
    expect_is(shared.docs, 'Shared-Documentation')

    
    
    # documentation
    
    
}


