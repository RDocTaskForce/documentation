#' @include Classes.R
#' @include Fun-documentation.R

get_S4_documentation <-
function( Class  #< A [classRepresentation] or a string representing a class.
        , ... #< passed on to getClassDef if `Class` is a character, discarded otherwise.
        ){
    if(inherits(Class, 'character'))
        Class <- getClassDef(Class, ...)
    if(!is(Class, 'classRepresentation'))
        stop('`Class` argument must be either classRepresentation,'  %<<%
             'obtained from getClass, or a character specifying the' %<<%
             'name of a class.')
    cname <- Class@className
    dname <- documentationMetaName(cname, packageSlot(Class))
    value <- get0(dname, envir=env, mode='S4', inherits=TRUE)
    if(!is.null(value)) return(value)

    dname <- documentationMetaName(cname,'')
    env   <- .classEnv(Class, topenv(parent.frame()), TRUE)
    value <- get0(dname, envir=env, mode='S4', inherits = FALSE)
    if(!is.null(value)) return(value)

}


setMethod('documentation', 'classRepresentation',
function( object    #< the classRepresentation object
        , ...       #< Currently ignored
        ){
    #' Get documentation for an S4 class.
    #'
    #' Documentation in subsequent packages takes precedence over
    #' the documentation in the package.
    #'
    #'
    #'
    cname <- object@className
    dname <- documentationMetaName(cname, pkg=attr(object, 'package'))
    value <- get0(dname, mode='S4', inherits=TRUE)
    if(!is.null(value)) return(value)

    dname <- documentationMetaName(cname,'')
    env   <- .classEnv(object, topenv(parent.frame()), TRUE)
    value <- get0(dname, envir=env, mode='S4', inherits = FALSE)
    if(!is.null(value)) return(value)
    stop("Documentation for class ", cname, " not found.")
})
if(F){
    object <- getClass('S4-Documentation')
}


set_S4_documentation <-
function( Class  #< A [classRepresentation] or a string representing a class.
        , docs    #< [S4-Documentation] The documentation for the class represented by `Class`.
        , where   = topenv(parent.frame())  #< where to store the documentation
        , verbose = getOption("documentation::verbose", FALSE) #< Should informational messages be shown?
        , ... #< passed on to getClassDef if `Class` is a character, discarded otherwise.
        ){
    if(inherits(Class, 'character'))
        Class <- getClassDef(Class, where=where, ...)
    if(!is(Class, 'classRepresentation'))
        stop('`Class` argument must be either classRepresentation,'  %<<%
             'obtained from getClass, or a character specifying the' %<<%
             'name of a class.')
    if(getPackageName(where) == packageSlot(Class))
        dname <- documentationMetaName(Class@className, '')
    else
        dname <- documentationMetaName(Class@className, packageSlot(Class))
    if(verbose)
        if(exists(dname, where, mode='S4', inherits=FALSE))
            message("documentation already exists; overwriting.")
    assign(dname, docs, envir=where)
}
setMethod('documentation<-', c('classRepresentation', 'S4-Documentation'),
function(object, value){
    set_S4_documentation(object, value)
})
setMethod('documentation<-', c('ANY', 'S4-Documentation'),
function(object, value){
    stop('S4-Documentation objects can only be set for objects'   %<<%
         'of, or inheriting from, the classRepresentation class.')
})

if(FALSE){#
#~ documentation.S4_Documentation <-
#~ S4_documentation( author      = person("Andrew", "Redd", email="Andrew.Redd@hsc.utah.edu")
#~                 , title       = "Documentation for S4 classes"
#~                 , description = "The Documentation S4 class provides the basis for all" %<<%
#~                                 "complete documentation in the documentation package system."
#~                 , seealso     = "\\code{\\link{Documentation}}"
#~                 , keywords    = "documentation"
#~                 )
#~ set_S4_documentation('S4-Documentation', documentation.S4_Documentation)


#~     expect_true(exists('.__Documentation__S4-Documentation'
#~                       , envir=asNamespace('documentation')
#~                       , mode='S4', inherits=FALSE))
#~     expect_is()

}


setClass('S4-Method-Documentation', contains='function-Documentation'
        , slots = c( signature = 'character'
                   , package.of.generic = 'character'
                   )
        )
if(F){
    x <-
    new('S4-Method-Documentation', name = 'documentation<-'
       , signature = c(object='classRepresentation', value='S4-Documentation')
       , title = 'Assign documentation to a S4 Class'
       )


    mdef <- GetMethod
}

S4_method_meta_name <-
function( f
        , signature
        ){
# Question:
# the methods are stored in an enclosing environment for the generic function.
# Should the documentation objects be included in that environment or
# in the environment/package where the method is being defined?
# If it is in the original environment of the generic function there
# should be a catalog of documentation stored externally so that all
# documentation objects can be accounted for.

}

set_S4_methods_documentation <-
function( docs      #< [S4-Method-Documentation] Documentation for the S4 method.
        , where   = topenv(parent.frame())  #< where to store the documentation
        , verbose = getOption("documentation::verbose", FALSE) #< Should informational messages be shown?
        , ...
        ){


}

