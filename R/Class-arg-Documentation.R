#' @include utils.R
#' @include Class-Vector.R

#' @export
arg_ <-
setClass( "arg-Documentation"
        , slots = c( name        = 'name'
                   , description = 'character'
                   , default     = 'Documentation-Default-Value'
                   , constraints = 'list'
                   )
        )
#' @export
setMethod('initialize', 'arg-Documentation',
    function( .Object
            , name                      #< [name|character] name of the argument
            , description               #< [character] description of the argument
            , default =                 #< default value
                 new('Documentation-No-Default-Value')
            , constraints = list()   #< list of constraints
            ){
        #! Create documentation for a function argument.
        .Object@name        <- as.name(name)
        .Object@description <- as.character(description)
        .Object@default     <- as(default, 'Documentation-Default-Value')
        .Object@constraints <- constraints
        return(.Object)
    })
arg <-
function( name                   #< name of the argument
        , description            #< [character] description of the argument
        , default                #< default value
        , ...                    #< named constraints
        ){
    #! Create documentation for a function argument, lazy version
    default <- if(missing(default))
        new('Documentation-No-Default-Value')
    else
        as(substitute(default), 'Documentation-Default-Value')
    arg_( name        = as.name(substitute(name))
        , description = description
        , default     = default
        , constraints = list(...)
        )
}

#' @exportClass ArgumentList
#' @S3method [ ArgumentList
#' @S3method [<- ArgumentList
#' @S3method c arg-Documentation
#' @S3method c ArgumentList
#' @S3method unique ArgumentList
setVector( element = "arg-Documentation"
         , Class   = "ArgumentList"
         )
#' @export
ArgumentList <- function(...){new('ArgumentList', list(...))}
AL <- ArgumentList

if(FALSE){#! @testing arg
    a <- new( "arg-Documentation"
            , name= 'testing'
            , description='a testing argument'
            , default=new('Documentation-No-Default-Value')
            )
    expect_identical(a@name       , as.name('testing'))
    expect_identical(a@description, 'a testing argument')
    expect_identical(a@default    , new('Documentation-No-Default-Value'))

    b <- arg_('testing', 'a testing argument')
    expect_identical(a,b)

    c <- arg(testing, 'a testing argument')
    expect_identical(a,c)


    d <- arg(testing, NA_character_, NULL)
    expect_identical(d@default, new('Documentation-Default-Value:NULL'))
    expect_identical(d@description, NA_character_)

    e <- arg(testing, NA, NULL)
    expect_identical(e@description, NA_character_)

    testing <- "SAY MY NAME!"
    b <- arg(name= testing, description='a testing argument')
    expect_identical(a,b)

    L <- new("ArgumentList")
    L[[1]] <- a
    L[[2]] <- b

    expect_is(L, 'ArgumentList')
    expect_equal(length(L), 2)

    M <- ArgumentList(a, b)
    expect_identical(L, M)

    expect_identical(c(M, c), AL(a,b,c))
    expect_identical(c(M, AL(c,d)), AL(a,b,c,d))
}
if(FALSE){#@testing unique.ArgumentList and [.ArgumentList
    a <- arg(a, 'first')
    b <- arg(b, 'second')
    c <- arg(c, 'last')

    x <- AL(a,b)

    expect_identical(y <- c(x, a, b, c), AL(a, b, a, b, c))
    expect_identical(unique(y), AL(a,b,c))

    expect_identical(y[c(1,3)], AL(a,a))
}
if(FALSE){#@testing c.arg-Documentation
    a <- arg(a, 'first')
    b <- arg(b, 'second')

    x <- c(a,b)
    expect_identical(x, AL(a,b))
}

