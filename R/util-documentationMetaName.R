#' @include setup.R

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
#' @seealso methodsPackageMetaName

    if(is.null(pkg)) pkg <- ''
    stopifnot(is.character(object.name))
    prefix <- 'Documentation'
    if(!is.null(subsystem))
        prefix <- paste0(prefix, '/', subsystem)
    methodsPackageMetaName(prefix, object.name, pkg)
}
