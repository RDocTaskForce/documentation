
#' A Vector with only one type of object
setVector <- 
function( element                                   #< name of the class that elements of the vector must inherit from.
        , Class   = paste0("Vector(", element, ")") #< Name of the class
        , ...                                       #< passed onto SetClass, excluding contains, validity
        ){
stopifnot(is.character(element) && length(element) == 1)
setClass( Class=Class, contains = 'list'
        , validity =  function(object){
                for(i in seq_along(object))
                    if(!inherits(object[[i]], element))
                        return(sprintf("Element of Vector at position %d is not a %s", i, element))
                return(TRUE)
            }
        , ...
        )
}
if(FALSE){#! @testing
    new.class <- setVector('name') # creates `Vector()`
    name.vector <- new.class()
    name.vector[[1]] <- as.name('a')
    name.vector[[2]] <- as.name('b')
    expect_equal(length(name.vector), 2)
    expect_true(validObject(name.vector))
    name.vector[[3]] <- 'c'
    expect_error(validObject(name.vector), "Element of Vector at position 3 is not a name")
    
}

