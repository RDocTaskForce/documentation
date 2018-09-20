#' @importFrom rlang is_empty
#' @include Classes.R
#' @include Fun-toRd.R

setMethod('toRd', 'BaseDocumentation',
function( obj
        , ...
        , exclude = character(0)
        ){
    "Convert Documentation to Rd format."
    to_tag <- function(name)
        Rd_tag(tag=name, getElement(obj, name))
    .exclude <- c('author', 'keywords', 'aliases', 'concepts', 'sections')
    
    slots <- setdiff(slotNames(obj), exclude)
    Rd <- structure(lapply(slots, to_tag), names = slots)
    Rd <- Filter(length, Rd)
    if (length(obj@author))
        Rd[['author']]   <- Rd_author(obj@author)
    if (!rlang::is_empty(obj@keywords) && !('keywords' %in% exclude))
        Rd[['keywords']] <- toRd(doc_get_keywords(obj))
    if (!rlang::is_empty(obj@aliases) && !('aliases' %in% exclude))
        Rd[['aliases']]  <- cl(lapply(doc_get_aliases(obj), Rd_alias), 'Rd')
    if (!rlang::is_empty(obj@concepts) && !('concepts' %in% exclude))
        Rd[['concepts']] <- Rd_tag(doc_get_concepts(obj), 'concept')

    toRd(Rd, ...)
})
if(FALSE){#! @testing
    null.object <- new('BaseDocumentation')

    object <- new( "BaseDocumentation"
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

    expect_equal(as.rd[['author']], "\\author{Andrew Redd \\email{andrew.redd@hsc.utah.edu} and Drew Blue}")
    expect_equal(as.rd[['title']], "\\title{Create function documentation}")
    expect_equal(as.rd[['keywords']], "\\keyword{internal}")
    expect_equal(as.rd[['aliases']], "\\alias{test-alias}")
}





