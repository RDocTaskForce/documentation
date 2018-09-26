#' @include utils.R

#' @export
setClass( "data.frame-Documentation", contains = "BaseDocumentation"
        , slots = c( source = "bibentry" )
        , prototype = prototype( keywords=new('Documentation-Keyword', 'data') )
        )
