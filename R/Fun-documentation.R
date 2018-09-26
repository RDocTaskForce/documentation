#' @include Classes.R

#' @title Retrieve documentation
#'
#' @description
#' Generic function for retrieving documentation for an object.
#' Inspects the type of object that is given to it and retrieves
#' documentation from the appropriate place.
#'
#' * function documentation is attached to the function.
#' * S4 documentation is stored in an independent object with an
#'   appropriately name.  The same for S4 derived classes such as
#'   <ReferenceClasses>.
#' @export
setGeneric( 'documentation'
          , valueClass = 'Documentation'
          , simpleInheritanceOnly = TRUE
          , function(object, ..., name=deparse(substitute(object))){
        if(!is.null(docs <- attr(object, 'documentation'))){
            if (!is(docs, 'Documentation'))
                doc_invalid(name)
            return(docs)
        }
        if(isS4(object))
            return(documentation(getClass(class(object)), ...))
        doc_dnf_error(name)
    }, signature =c('object'))
if(FALSE){#! @testing
    test_function <- function(x){
        return(x)
    }
    expect_error( documentation(test_function)
                , class='documentation-error-dnf'
                )

    attr(test_function, "documentation") <- "An invalid documentation"
    expect_error( documentation(test_function)
                , class = 'documentation-error-invalid'
                )
    attr(test_function, "documentation") <-
        function_documentation('test_function'
                              , title="A test function"
                              )
    expect_is(documentation(test_function), 'Documentation')
}

#' Replace documentation
#'
#' Generic for setting documentation.  Similar to the `documentation()`
#' function, this is a generic whose behavior differs depending on the
#' type of object that is beeing passed to it.  However, unlike the
#' retrieval function this cannot set documentation for individual
#' instances or objects, but must operate on the class definition for
#' S4 objects.
#' @export
setGeneric( 'documentation<-', signature=c('object', 'value')
          , simpleInheritanceOnly = FALSE
          , function(object, value){
                if (!is(value, 'Documentation')) doc_invalid()
                standardGeneric('documentation<-')
            })

setMethod('documentation<-', c('ANY', 'Documentation'),
function( object, value
        ){
    complete  = getOption("documentation::complete" , NULL)
    overwrite = getOption("documentation::overwrite", 'message')

    if (!is.null(overwrite) && !is.null(attr(object, 'documentation')))
        doc_overwrite(NULL, overwrite)
    if (!is.null(complete) && !is_complete(value))
        doc_incomplete(NULL, complete)

    attr(object, 'documentation') <- value
    object
})
if(FALSE){#! @testing
    x <- 1
    y <- new('BaseDocumentation', title='testing')

    documentation(x) <- y
    expect_identical(attr(x, 'documentation'), y)
    expect_identical(documentation(x), y)

    expect_error( documentation(x) <- 'this should not work'
                , class = 'documentation-error-invalid'
                )
    expect_message( documentation(x) <- y
                  , class = 'documentation-message-overwrite'
                  )
}


