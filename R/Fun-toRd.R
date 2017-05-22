#' @include options.R 
#' @include util-str_rep.R 

#' @importFrom tools toRd
setGeneric('toRd', tools::toRd)

set_option_documentation( "documentation::toRd::indent"
   , description = "Determines if code should be indented when formatted.  Should default to FALSE when unset."
   )


#~ defaults$toRd$indent.with <- '    '

set_option_documentation( "documentation::toRd::indent.with"
   , description = "Determines what to indent with, when getOption('documentation::format_md::indent') is TRUE."
   )


#~ defaults$format_md$collapse.lines <- FALSE

set_option_documentation("documentation::toRd::collapse.lines"
   , description = "should documentation functions return a single " %\%
                   "string (TRUE) or a array of strings (FALSE) " %\%
                   "representing the lines of documentation."
   )

#~ defaults$format_md$collapse.with <- '\n'
set_option_documentation("documentation::toRd::collapse.with"
   , description = "when \\code{getOption(documentation::format_md::collapse.lines)}" %\%
                   "is \\code{TRUE} what the lines should be separated with."
   )
Rd_tag  <- function(content, name=deparse(substitute(content))){
    if(!identical(class(content), 'character')) content <- toRd(content)
    if(length(name)==0 || length(content)==0) return(character(0))
    if(length(content) == 1)
        return( sprintf("\\%s{%s}", name, content) )
    else 
        return( c(sprintf("\\%s{", name), content, "}") )
}
if(FALSE){#! @testing
    expect_equal(Rd_tag('my name', 'name'), "\\name{my name}")
    expect_equal(Rd_tag('test', NULL), character(0))
    expect_equal( Rd_tag(c('line1', 'line2'), 'name')
                , c('\\name{', 'line1', 'line2', '}')
                )
    name <- 'testing'
    expect_equal(Rd_tag(name), '\\name{testing}')
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
if(FALSE){#!@testing
    object <- citation() %>% structure(class='bibentry')
    toRd(object)
    
    toRd(object, style='documentation')
    
    
    
    
    
    bs <- bibstyle()
    bs %>% ls()
    bs %$% collapse
    
    
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
