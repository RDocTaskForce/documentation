#' @include Class-References.R
#' @importFrom utils person
#' @import methods

#TODO
#' only text stored in an HTML5/XHTML compliant format.
setClass( "FormattedText", contains = 'list')

#TODO
#' contains formatted text and code chunks
setClass( "Prose", contains = 'list')


setOldClass('person')

setClass( "Documentation"
        , slots = c( author      = "person"
                   , title       = "character"
                   , description = "FormattedText"
                   , references  = "References"
                   , seealso     = "FormattedText"
                   , examples    = "Prose"
                   , keywords    = "character"
                   )
        )


