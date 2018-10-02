#' @include utils.R

#' @export
data_documentation <-
setClass( "data-Documentation", contains = "BaseDocumentation"
        , slots = c( name = 'name'
                   , source = "bibentry" )
        , prototype = prototype( keywords=new('Documentation-Keyword', 'data') )
        )

# setMethod("initialize", 'data-Documentation', function())

