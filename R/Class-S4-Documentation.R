#' @include utils.R
#' @include Class-function-Documentation.R
#' @include Class-Vector.R

setClass('slot-Documentation', contains = 'arg-Documentation'
        , slots= c(valid.class='character')
        )

setVector( element = "slot-Documentation"
         , Class   = "Slots-Documentation"
         )

S4_documentation <-
setClass( 'S4-Documentation', contains='BaseDocumentation'
        , slots = c( Slots = 'Slots-Documentation'
                   , documented.class = 'character'
                   , origin.package   = 'character'
                   )
        )
#~ setMethod('initialize', 'S4-Documentation',
#~     function(.Object, class, pkg, ...){
#~         .Object <- callNextMethod(.Object...)
#~     })
