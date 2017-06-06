#' @include Vector.R
#' @include Class-Documentation-Keyword.R
#' @include Class-FormattedText.R
#' @importFrom utils person
#' @import methods


#TODO
#' @title contains formatted text and code chunks
setClass( "Prose", contains = 'list')
#TODO: expand prose with validator and possibly initializer to check for 
#^ valid code.

setVector( element = "Prose"
         , Class   = "SectionList"
         )

#' @importFrom utils bibentry person
setOldClass('bibentry')
setOldClass('person')

setClass( "Documentation"
        , slots = c( author      = "person"
                   , title       = "character"
                   , description = "FormattedText"
                   , references  = "bibentry"
                   , seealso     = "FormattedText"
                   , examples    = "Prose"
                   , keywords    = "Documentation-Keyword"
                   , aliases     = "character"
                   , concepts    = "character"
                   , sections    = "SectionList"
                   )
        , prototype = list( author     = person()
                          , references = bibentry()
                          )
        )

setMethod("initialize", 'Documentation', 
    function( .Object
            , ...
            , author      = NULL
            , description = NULL
            , keywords    = NULL
            , seealso     = NULL
            , examples    = NULL
            , references  = NULL
            )
    {
        .Object <- callNextMethod( .Object, ...)
        if (!is.null(keywords   ))  .Object@keywords    <- new("Documentation-Keyword", keywords   )
        if (!is.null(description))  .Object@description <- new("FormattedText"        , description)
        if (!is.null(seealso    ))  .Object@seealso     <- new("FormattedText"        , seealso    )
        if (!is.null(author     ))  .Object@author      <- as.person(author)
        if (!is.null(examples   )){ .Object@examples  <- 
            if(inherits(examples, 'list')){
                new('Prose', examples)
            } else {
                new('Prose', list(examples))
            }
        }
        if (!is.null(references ))
            if(inherits(references, 'citation')){
                .Object@references <- structure(references, class='bibentry')
            } else if(inherits(references, 'bibentry')){
                .Object@references <- references
            } else {
                .Object@references <- new('References', references)
            }
        return(.Object)
    })
if(FALSE){#!@testing
    x <- new('Documentation')
    expect_identical(x@author, person())
    expect_identical(x@title, character(0))
    
    x <- new('Documentation'
            , author     = person('Andrew', 'Redd')
            , references = citation()
            )
    expect_equal(x@author, person('Andrew', 'Redd'))
}

setGeneric("documented", 
function(object, ...){
    return( 
        structure( object
                 , docs   = new(paste0(class(object)[[1]], '-Documentation'), ...)
                 )
          )
    
})
if(FALSE){
    object <- function(msg="hello world"){print(msg)}
    
}

setMethod("as.list", 'Documentation',
function(x, ...){
    structure( lapply(slotNames(x), getElement, object=x)
             , names = slotNames(x)
             )
})
if(FALSE){#! @testing
    x <- 
    object <- new( "Documentation"
             , author      = c( person('Andrew', 'Redd', email='andrew.redd@hsc.utah.edu')
                              , person('Drew'  , 'Blue')
                              )
             , title       = 'Create function documentation'
             , description = stringi::stri_rand_lipsum(3)
             , seealso     = '\\link{documentation-package}'
             , keywords    = 'internal'
             , aliases     = 'test-alias'
             , references  = citation()
             )
    object.as.list <- as.list(object)
    expect_is(object.as.list, 'list')
    expect_equal(names(object.as.list), slotNames(object))
    
    
}

