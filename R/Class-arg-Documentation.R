#' @include Vector.R

setClass( "arg-Documentation"
        , slots = c( name        = 'name'
                   , description = 'character'
                   , default     = 'expression'
                   , constraints = 'list'
                   )
        )
arg <- 
function( name                   #< name of the argument
        , description            #< [character] description of the argument
        , default = expression() #< default value
        , ...                    #< named constraints
        ){
    #! Create documenation for a function argument
    new("arg-Documentation", name=name, description=description, default=default, constraints=list(...))
}

setVector( element = "arg-Documentation"
         , Class   = "ArgumentList"
         )
ArgumentList <- function(...){new('ArgumentList', list(...))}


if(FALSE){#! @testing
    a <- new("arg-Documentation", name= as.name('testing'), description='a testing argument')
    expect_equal(a@default, expression())
    expect_equal(a@name, as.name('testing') )
    expect_equal(a@description, 'a testing argument')
    
    b <- arg(name= as.name('testing'), description='a testing argument')
    expect_identical(a,b)
    
    L <- new("ArgumentList")
    L[[1]] <- a
    L[[2]] <- b
    
    expect_is(L, 'ArgumentList')
    
    M <- ArgumentList(a, b)
    expect_identical(L, M)
}




