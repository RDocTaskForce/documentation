#' @include utils.R

#' A Vector with only one type of object
setVector <-
function( element                                   #< name of the class that elements of the vector must inherit from.
        , Class   = paste0("Vector(", element, ")") #< Name of the class
        , ...                                       #< passed onto SetClass, excluding contains, validity
        , contains = character()
        , where = topenv(parent.frame())
        ){
stopifnot(is.character(element) && length(element) == 1)
#' @export
val <-
setClass( Class=Class, contains = c(contains, 'list')
        , validity =  function(object){
                for(i in seq_along(object))
                    if(!inherits(object[[i]], element))
                        return(sprintf("Element of Vector at position %d is not a %s", i, element))
                return(TRUE)
            }
        , ..., where=where)
setAs(element, Class, function(from){
    new(Class, list(from))
}, where=where)
return(val)
}
if(FALSE){#! @testing
    new.class <- setVector('name', where=globalenv()) # creates `Vector()`
    expect_is(new.class, "classGeneratorFunction")

    name.vector <- new.class()
    name.vector[[1]] <- as.name('a')
    name.vector[[2]] <- as.name('b')
    expect_equal(length(name.vector), 2)
    expect_true(validObject(name.vector))
    name.vector[[3]] <- 'c'
    expect_error(validObject(name.vector), "Element of Vector at position 3 is not a name")

    expect_is(.undefined, 'name')
    expect_identical( as(.undefined, "Vector(name)")
                    , new.class(list(.undefined))
                    )
    removeClass(new.class@className, where=globalenv())
}

