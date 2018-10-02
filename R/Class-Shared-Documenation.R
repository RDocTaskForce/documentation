
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
setMethod("initialize", "Shared-Documentation",
    function(.Object, docs, ...){
        assert_that(is(docs, 'Documentation'))
        .Object$docs <- docs
        return(.Object)
    })
if(FALSE){
    doc <- shared(function_documentation())
    expect_is_exactly(doc, 'Shared-Documentation')
}

### Documentation → Shared-Documentation ≝ #####
setAs('Documentation', 'Shared-Documentation', function(from)shared(docs=from))
### Shared-Documentation ≜ Documentation #####
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
}

### Functions #####

doc_combine <- function(x, ...){



}
if(FALSE){




}




