#' @include Class-Documentation.R

#TODO 

#' Retrieve documentation
#' 
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
          , function(object, ...){
        if(!is.null(docs <- attr(object, 'documentation')))
            return(docs)
        if(isS4(object))
            return(documentation(getClass(class(object)), ...))
        message('Documentation not found!.')
    })

#' Replace documentation
#' 
#' Generic for setting documentation.  Similar to the `documentation()`
#' function, this is a generic whose behavior differs depending on the
#' type of object that is beeing passed to it.  However, unlike the
#' retrieval function this cannot set documentation for individual 
#' instances or objects, but must operate on the class definition for 
#' S4 objects. 
#' @export
setGeneric( 'documentation<-'
          , simpleInheritanceOnly = TRUE
          , function(object, value){
        stop('Documentation can only be set with objects ' %\% 
             'of, or inheriting from, the `Documentation` class.')
    })

setMethod('documentation<-', c('ANY', 'Documentation'), 
function(object, value){
    if (getOption("documentation::verbose", FALSE) && !is.null(attr(object, 'documentation')))
        message("documentation already exists, replacing documentation")
    attr(object, 'documentation') <- value
    object
})
if(FALSE){#! @testing
    x <- 1
    y <- new('Documentation', title='testing')
    
    documentation(x) <- y
    expect_identical(attr(x, 'documentation'), y)
    expect_identical(documentation(x), y)
    
    expect_error( documentation(x) <- 'this should not work'
                , 'Documentation can only be set with objects ' %\% 
                  'of, or inheriting from, the `Documentation` class.'
                )
    
}
#' Create a cannonical name for independent documentation objects
documentationMetaName <- 
function( object.name   #< [character] name of the object that is being documented such as class
        , pkg=attr(object.name, 'package')  #< [character] package name, optional.
        , subsystem = NULL                  #< [character] documentation subsystem.                     
        ){
#' This function supports S4 and similar class documentation, where the
#' documentation needs to exist and be associated with the definition
#' not be attached to the individual objects instances of that class.
#' 
#' The `pkg` argument should be specified if and only if the 
#' documentation exists outside the package that defines the class.
#' this does 
#' @seealso <methodsPackageMetaName>

    if(is.null(pkg)) pkg <- ''
    stopifnot(is.character(object.name))
    prefix <- 'Documenation'
    if(!is.null(subsystem))
        prefix <- paste0(prefix, '/', subsystem)
    methodsPackageMetaName(prefix, object.name, pkg)
}
