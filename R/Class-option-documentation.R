#' @include Class-Documentation.R
#' @include Class-Documentation-Default-Value.R

option_documentation <- 
setClass('option-Documentation', contains = 'Documentation'
        , slots = c( key         = 'character'
                   , default     = 'Documentation-Default-Value'
                   , constraints = 'list'
                   )
        )
setMethod('initialize', 'option-Documentation',
    function( .Object
            , key                       #< [character] name of the argument
            , description               #< [character] description of the argument
            , constraints = list()      #< list of constraints
            ){
        #! Create documenation for a function argument.
        .Object@key         <- as.character(key)
        .Object@description <- FormattedText(as.character(description))
        .Object@constraints <- constraints
        return(.Object)
    })

if(FALSE){#! @testing
    docs <- option_documentation('anOption', 'a description')
    
}

set_option_documentation <- 
function( key #< [character] Option name.
        , ... #< passed on to `<option_documentation>` to create the documentation object.
        , where   = topenv(parent.frame())  #< where to store the documentation
        , pkg     = getPackageName(where)   #< package that option refers to.
        , verbose = getOption("documentation::verbose", FALSE) #< Should informational messages be shown?
        ){
    dname <- documentationMetaName(key, pkg, subsystem ='Options')
    docs  <- option_documentation(key=key, ...)

    if(verbose)
        if(exists(dname, where, mode='S4', inherits=FALSE))
            message("documentation already exists; overwriting.")
    assign(dname, docs, envir=where)
}


#TODO: Arguments should automatically inherit description from option 
#       when argument defaults to a getOption.

if(FALSE){
    o <- new('option-Documentation', 'anOption', 'a description')
    expect_is(o, 'option-Documentation')
    expect_is(o, 'Documentation')
    expect_identical(o@key, 'anOption')
    expect_identical(o@description, FormattedText('a description'))
    
    o <- option_documentation('anOption', 'a description')
    expect_is(o, 'option-Documentation')
    expect_is(o, 'Documentation')
    expect_identical(o@key, 'anOption')
    expect_identical(o@description, FormattedText('a description'))
    
}

