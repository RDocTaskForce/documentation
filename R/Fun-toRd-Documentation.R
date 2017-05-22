#' @include Class-Documentation.R
#' @include Fun-format_Rd.R

setMethod('toRd', 'Documentation',
function( obj
        , ...
        ){
    "Convert Documentation to Rd format."
    to_tag <- function(name) Rd_tag( getElement(object, name) , name)
    
    slots <- slotNames(object) 
    Rd <-lapply(slots, to_tag) %>% structure(names = slots)
    
    Rd$author   <- Rd_tag('author', comma_list(toRd(object@author)))
    Rd$keywords <- toRd(object@keywords)
    Rd$aliases  <- Rd_tag(object@aliases, 'alias')
    Rd$concepts <- Rd_tag(object@concepts, 'concept')
    Filter(length, Rd)
})
if(FALSE){#! @testing
    null.object <- new('Documentation')
    
    object <- new( "Documentation"
                 , author      = c( person('Andrew', 'Redd', email='andrew.redd@hsc.utah.edu')
                                  , person('Drew'  , 'Blue')
                                  )
                 , title       = 'Create function documentation'
                 , description = stringi::stri_rand_lipsum(3)
                 , seealso     = '\\link{documentation-package}'
                 , keywords    = 'internal'
                 , aliases     = 'test-alias'
                 , concepts    = 'test concept'
                 , references  = citation()
                 )
    as.rd <- toRd(object)
    expected.names <- c( 'author', 'title', 'description', 'seealso', 'keywords'
                       , 'aliases', 'references', 'concepts'
                       )
    expect_true(all(names(as.rd) %in% expected.names))
    expect_true(all(expected.names %in% names(as.rd)))
    
    expect_equal(as.rd$title, "\\title{Create function documentation}")
    expect_equal(as.rd$keywords, "\\keyword{internal}")
    expect_equal(as.rd$aliases, "\\alias{test-alias}")
    
}





