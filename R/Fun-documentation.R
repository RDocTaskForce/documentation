#' @include Classes.R

### Generic: documentation #####
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

### Generic: documentation<- #####
#' Set or Replace documentation
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

### Method: documentation<-Any,Documentation #####
#' @export
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

### set_documentation #####
#' Set documentation
set_documentation <-
function( object, doc
        , name=substitute(object)
        , envir=environment(object) %||% parent.frame()
        , ...){
    force(envir)
    force(name)
    assert_that( is(doc, 'Documentation')
               , is.name(name)
               , is.environment(envir)
               )
    set_documentation_(object, doc, name=name, envir=envir)
}

### Generic: set_documentation_ #####
setGeneric( 'set_documentation_'
          , signature = c('object', 'doc', 'name', 'envir'),
    function( object, doc, name, envir
            , ...) standardGeneric('set_documentation_')
    )

### Method: set_documentation_,Any,Documentation #####
setMethod('set_documentation_', signature = c('ANY', 'Documentation'),
    function( object, doc
            , name
            , envir
            , ...
            , .infer=TRUE
            ){
        assert_that( is.flag(.infer))

        if (.infer){
            if (.is_undefined(doc@name))
                doc@name <- name
            if (!length(. <- doc_get_aliases(doc)) || deparse(name) %!in% .)
                doc@aliases <- c(., deparse(name))
        }
        documentation(object) <- doc
        assign(deparse(name), value=object, envir = envir)
        invisible(doc)
    })
if(FALSE){#@testing
    a <- letters

    docs <- data_documentation(title="Letters big and small", description = "Small letters"
                              , source = bibentry() )

    expect_error(documentation(a))
    set_documentation(a, docs)

    d2 <- docs
    d2@aliases <- 'a'
    d2@name <- as.name('a')
    expect_identical(documentation(a), d2)

    A <- LETTERS
    set_documentation(A, docs)
    d3 <- docs
    d3@aliases <- 'A'
    d3@name <- as.name('A')
    expect_identical(documentation(A), d3)

    expect_message(set_documentation(A, documentation(a)))
    d4 <- d2
    d4@aliases <- c('a', 'A')
    expect_identical(documentation(A), d4)
}

