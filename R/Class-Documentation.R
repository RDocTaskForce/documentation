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
        )

