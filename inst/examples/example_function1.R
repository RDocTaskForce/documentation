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
