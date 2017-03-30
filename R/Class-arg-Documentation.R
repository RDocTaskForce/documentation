#' @include Vector.R
#' @include Fun-format_md.R
#' @include Fun-format_Rd.R

arg_ <-
setClass( "arg-Documentation"
        , slots = c( name        = 'name'
                   , description = 'character'
                   , default     = 'Documentation-Default-Value'
                   , constraints = 'list'
                   )
        )
setMethod('initialize', 'arg-Documentation',
    function( .Object
            , name                      #< [name|character] name of the argument
            , description               #< [character] description of the argument
            , default =                 #< default value
                 new('Documentation-No-Default-Value')
            , constraints = list()   #< list of constraints
            ){
        #! Create documenation for a function argument.
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
    #! Create documenation for a function argument, lazy version
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


setVector( element = "arg-Documentation"
         , Class   = "ArgumentList"
         )
ArgumentList <- function(...){new('ArgumentList', list(...))}


if(FALSE){#! @testing
    a <- new("arg-Documentation", name= 'testing', description='a testing argument', default=new('Documentation-No-Default-Value'))
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
    
    b <- arg(name= as.name('testing'), description='a testing argument')
    expect_identical(a,b)
    
    L <- new("ArgumentList")
    L[[1]] <- a
    L[[2]] <- b
    
    expect_is(L, 'ArgumentList')
    expect_equal(length(L), 2)
    
    M <- ArgumentList(a, b)
    expect_identical(L, M)
}

setMethod('format_Rd', 'arg-Documentation', 
function(object, ...){
    #Question: does thise need to include more details like default value?
    sprintf("\\item{%s}{%s}", as.character(object@name), object@description)
})

if(FALSE){#! @testing
    a <- new("arg-Documentation", name= 'testing', description='a testing argument', default=new('Documentation-No-Default-Value'))
    expect_identical(format_Rd(a), "\\item{testing}{a testing argument}")
}


setMethod('format_Rd', 'ArgumentList', 
function( object
        , ...
        , indent          = default(indent        )
        , indent.with     = default(indent.with   )
        , collapse.lines  = default(collapse.lines)
        , collapse.with   = default(collapse.with )
        ){
    formatted.args <- sapply(object, format_Rd, ...)
    if(indent)
        formatted.args <- paste0(indent.with, formatted.args)
    lines <- 
        c( '\\arguments{' 
         , formatted.args
         , '}'
         )
    if(collapse.lines)
        lines <- paste(lines, collapse = collapse.with)
    return(lines)
})

if(FALSE){#! @testing
    trace("format_Rd", browser, signature='ArgumentList')
    
    a <- new("arg-Documentation", name= 'testing', description='a testing argument', default=new('Documentation-No-Default-Value'))
    b <- arg_('testing', 'a testing argument')
    
    object <- 
        ArgumentList( arg_('x', 'an argument')
                    , arg_('y', 'another argument')
                    )

    expect_equal( format_Rd(object, indent = FALSE, collapse.lines=FALSE)
                , c( '\\arguments{'
                   , '\\item{x}{an argument}'
                   , '\\item{y}{another argument}'
                   , '}'
                   )
                )
    expect_equal( format_Rd(object, indent = FALSE, collapse.lines=FALSE)
                , c( '\\arguments{'
                   , '\\item{x}{an argument}'
                   , '\\item{y}{another argument}'
                   , '}'
                   )
                )
    expect_equal( format_Rd(object, indent = TRUE, indent.with='    '
                                  , collapse.lines=FALSE)
                , c( '\\arguments{'
                   , '    \\item{x}{an argument}'
                   , '    \\item{y}{another argument}'
                   , '}'
                   )
                )
    expect_equal( format_Rd(object, indent = TRUE, indent.with='\t'
                                  , collapse.lines=FALSE)
                , c( '\\arguments{'
                   , '\t\\item{x}{an argument}'
                   , '\t\\item{y}{another argument}'
                   , '}'
                   )
                )
    expect_equal( format_Rd(object, indent = FALSE, indent.with='    '
                           , collapse.lines=TRUE, collapse.with='\n'
                           )
                , paste( '\\arguments{'
                       , '\\item{x}{an argument}'
                       , '\\item{y}{another argument}'
                       , '}'
                       , sep='\n')
                )
    expect_equal( format_Rd(object, indent = TRUE, indent.with='    '
                           , collapse.lines=TRUE, collapse.with='\n'
                           )
                , paste( '\\arguments{'
                       , '    \\item{x}{an argument}'
                       , '    \\item{y}{another argument}'
                       , '}'
                       , sep='\n')
                )
}

