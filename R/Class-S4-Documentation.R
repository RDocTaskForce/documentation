#' @include Class-function-Documentation.R
#' @include Vector.R

setClass('slot-Documentation', contains = 'arg-Documentation')

setVector( element = "slot-Documentation"
         , Class   = "Slots-Documentation"
         )

S4_documentation <- 
setClass( 'S4-Documentation', contains='Documentation'
        , slots = c( Slots = 'Slots-Documentation' )
        )

setClass('S4-Method-Documentation', contains='function-Documentation'
        , slots = c(signature= 'character')
        )

docs.S4_documentation <- 
S4_documentation( author      = person("Andrew", "Redd", "Andrew.Redd@hsc.utah.edu")
                , title       = "Documentation for S4 classes"
                , description = ""
                , seealso     = "Documentation"
                , examples    = "Prose"
                , keywords    = "Documentation-Keyword"
                , alias       = "character"
                , sections    = "SectionList"
                )
                


