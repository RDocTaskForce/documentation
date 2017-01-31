#' @include Class-References.R
#' @include Class-Documentation-Keyword.R
#' @include Class-FormattedText.R
#' @importFrom utils person
#' @import methods


#TODO
#' contains formatted text and code chunks
setClass( "Prose", contains = 'list')

setVector( element = "Prose"
         , Class   = "SectionList"
         )

setOldClass('person')

setClass( "Documentation"
        , slots = c( author      = "person"
                   , title       = "character"
                   , description = "FormattedText"
                   , references  = "References"
                   , seealso     = "FormattedText"
                   , examples    = "Prose"
                   , keywords    = "Documentation-Keyword"
                   , alias       = "character"
                   , sections    = "SectionList"
                   )
        , prototype = list( author = person() )
        )

setMethod("initialize", 'Documentation', 
    function( .Object
            , ...
            , author      = NULL
            , description = NULL
            , keywords    = NULL
            , seealso     = NULL
            )
    {
        .Object <- callNextMethod( .Object, ...)
        if (!is.null(keywords   ))  .Object@keywords    <- new("Documentation-Keyword", keywords   )
        if (!is.null(description))  .Object@description <- new("FormattedText"        , description)
        if (!is.null(seealso    ))  .Object@seealso     <- new("FormattedText"        , seealso    )
        return(.Object)
    })

if(FALSE){#!@testing
    x <- new('Documentation')
    expect_identical(x@author, person())
    expect_identical(x@title, character(0))
}


