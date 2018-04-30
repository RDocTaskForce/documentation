#' @include options.R 
#' @include util-str_rep.R 

#' @export
#' @importFrom tools toRd
setGeneric('toRd', tools::toRd)

set_option_documentation( "documentation::toRd::indent"
   , description = "Determines if code should be indented when formatted.  Should default to FALSE when unset."
   )


set_option_documentation( "documentation::toRd::indent.with"
   , description = "Determines what to indent with, when getOption('documentation::toRd::indent') is TRUE."
   )


set_option_documentation("documentation::toRd::collapse.lines"
   , description = "should documentation functions return a single " %\%
                   "string (TRUE) or a array of strings (FALSE) " %\%
                   "representing the lines of documentation."
   )

set_option_documentation("documentation::toRd::collapse.with"
   , description = "when \\code{getOption(documentation::toRd::collapse.lines)}" %\%
                   "is \\code{TRUE} what the lines should be separated with."
   )

Rd_tag  <- function(content, name=deparse(substitute(content))){
    if(length(name) != 1 || !is(name, 'character')) stop("Rd tag name must be a single character.")
    if(!identical(class(content), 'character')) content <- toRd(content)
    if(length(content)==0) return(character(0))
    if(length(content) == 1)
        return( sprintf("\\%s{%s}", name, content) )
    else 
        return( c(sprintf("\\%s{", name), content, "}") )
}
if(FALSE){#! @testing
    expect_error(Rd_tag('test', NULL), "Rd tag name must be a single character")
    expect_error(Rd_tag('test', c('a', 'b')), "Rd tag name must be a single character")
    expect_error(Rd_tag('test', 1), "Rd tag name must be a single character")
    expect_equal(Rd_tag('my name', 'name'), "\\name{my name}")
    expect_equal( Rd_tag(c('line1', 'line2'), 'name')
                , c('\\name{', 'line1', 'line2', '}')
                )
    name <- 'testing'
    expect_equal(Rd_tag(name), '\\name{testing}')
    
    obj <- new('FormattedText', stringi::stri_rand_lipsum(3))
    as.tag <- Rd_tag(obj)
    expect_is(as.tag, 'character')
    expect_length(as.tag, 7)
    expect_identical(as.tag[c(1,7)], c('\\obj{', '}'))
}

#' @export
toRd.person <- 
function( obj
        , ...
        , include = c('given', 'family', 'email')
        ){
    format( obj, include = include
          , braces  = list(email = c('\\email{', '}'))
          )
}
if(FALSE){#! @testing
    object <- person('Andrew', 'Redd', email='andrew.redd@hsc.utah.edu')
    expect_equal(toRd(object), 'Andrew Redd \\email{andrew.redd@hsc.utah.edu}')
    
    object <-c( person('First' , 'Author', email='me1@email.com')
              , person('Second', 'Author', email='me2@email.com') 
              )
    expect_equal(toRd(object), c('First Author \\email{me1@email.com}'
                                , 'Second Author \\email{me2@email.com}'
                                ) )
    expect_equal(toRd( c( person('Andrew', 'Redd', email='andrew.redd@hsc.utah.edu')
                        , person('Drew'  , 'Blue')
                        ) )
                , c( 'Andrew Redd \\email{andrew.redd@hsc.utah.edu}'
                   , 'Drew Blue'
                   ) 
                )
               
        

}

tools::bibstyle('documentation', collapse = collapse, .init=TRUE)
if(FALSE){#!@testing documentation bibstyle
    object <- citation() %>% structure(class='bibentry')
    default.style <- toRd(object, style='JSS')
    doc.style     <- toRd(object, style='documentation')
    
    expect_true(default.style != doc.style)
}


setMethod('toRd', 'Documentation-Keyword', function( obj, ...)sprintf("\\keyword{%s}", obj@.Data))
if(FALSE){#! @testing
    obj <- new('Documentation-Keyword', c('utilities', 'character'))
    expect_equal(toRd(obj), c('\\keyword{utilities}', '\\keyword{character}'))
}

setMethod('toRd', 'FormattedText', 
function( obj
        , ...
        , add.blank.lines = TRUE
        ){
    #! Convert formatted text into Rd lines.
    #! 
    #! \\note{ Assumes that each element of the text is a paragraph.)
    if( length(obj) == 0) return(character(0))
    else if( length(obj) == 1) return(obj@.Data)
    else return( head(interleave(obj, rep('', length(obj))),-1) )

})
if(FALSE){#! @testing
    obj <- FormattedText()
    expect_identical(toRd(obj), character(0))
    
    obj <- FormattedText('Hello world!')
    expect_identical(toRd(obj), 'Hello world!')
    expect_false(identical(toRd(obj), obj))
    
    obj <- FormattedText(stringi::stri_rand_lipsum(3))
    as.rd <- toRd(obj)
    expect_equal(length(as.rd), 5 )
    expect_is(as.rd, 'character')
}


#' @export
toRd.vector <- function(obj, name=deparse(substitute(obj)), ...){
    if (length(obj) == 0) return(character(0))
    if(is.atomic(obj)) Rd_tag(as.character(obj), name=name)
    Rd_tag(sapply(obj, toRd), name=name)
}
if(FALSE){#@testing
    obj <- new('FormattedText', stringi::stri_rand_lipsum(3))
    expect_is(obj, 'vector')
    as.rd <- toRd(obj, 'description')
    
}

