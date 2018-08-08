#' @include utils.R

setClass( "data.frame-Documentation", contains = "Documentation"
        , slots = c( source = "bibentry" )
        , prototype = prototype( keywords=new('Documentation-Keyword', 'data') )
        )
