# example_function1.R


#' This is the title
#'
#' This is the description block.
#' It takes multiple lines.
example_function1 <-
function( x #< inline documentation for x
        , y
        ){
    #' @param y explicit documentation for y
    paste(x, y, sep='-')
    #' @return explicit return.
}

#' Example function 2
#'
#' This is used to check for relative comment association.
example_function2 <-
function( x #< The x argument description.
        , y #< The y argument description
            #< takes 2 lines.
        ){
    x+y
    #< an orphaned relative comment.
}


#' An example character vector
#'
#' This example is used to test extracting documentation from
#' a character vector which typically does not contain
#' a source reference.
example_character <-
    c( "first"  #< the first element
     , "second" #< the second element
     )
