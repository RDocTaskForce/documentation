#' @importFrom rlang is_empty
#' @include Classes.R
#' @include Fun-toRd.R
#' @importFrom rlang is_empty


.default.basedocumentation.order <-
    c( 'name', 'aliases', 'concepts', 'title', 'author'
     , 'description', 'usage', 'arguments'
     , 'sections'
     , 'value', 'seealso'
     , 'references'
     , 'examples'
     , 'keywords'
     )


slot_to_tag <- function(slot, obj, ..., control=list()){
    elem <- getElement(obj, slot)
    if (length(elem) == 0) return(Rd())
    content <- toRd( elem, ...)
    Rd_tag_(slot, content, control=control)
}

setMethod('toRd', 'BaseDocumentation',
function( obj                     #< Documentation Object.
        , ...                     #< Currently Ignored, but included for compatability.
        , exclude = character(0)  #< Specific slots in `obj` to exclude from output.
        , control = list()        #< Formatting control parameters.
        ){
    "Convert Documentation to Rd format."
    to_tag <- function(name)
        if (length(elem <- getElement(obj, name)))
            Rd_tag(tag=name, content=toRd(elem, ...), ...)
    .exclude <- c('author', 'keywords', 'aliases', 'concepts', 'sections')

    slots <- setdiff(slotNames(obj), c(.exclude, exclude))
    rd <- structure(lapply(slots, slot_to_tag, obj=obj, control=control), names = slots)
    rd <- Filter(length, rd)

    rd[['author']]   <- Rd_author(obj@author) %if% (length(obj@author))
    rd[['keywords']] <- toRd(doc_get_keywords(obj)) %if% (!is_empty(obj@keywords) && !('keywords' %in% exclude))
    rd[['aliases']]  <- Rd_lines(lapply(doc_get_aliases(obj), Rd_alias)) %if% (!is_empty(obj@aliases) && !('aliases' %in% exclude))
    if (!rlang::is_empty(. <- obj@concepts) && !('concepts' %in% exclude)) {
        rd[['concepts']] <- Rd_lines(lapply(., Rd_concept))
    }
    order <- get_option( "Documentation::BaseDocumentation::documentation-order"
                       , .default.basedocumentation.order
                       )
    order <- unique(c(order, names(rd)))
    order <- intersect(order, names(rd))
    rd <- rd[order]
    Rd_lines(rd)
})
if(FALSE){#! @testing
    null.object <- new('BaseDocumentation')

    description <- withr::with_seed(20180921, stringi::stri_rand_lipsum(3))
    description <- Rd_canonize( Rd(collapse(description, '\n\n'))
                              , control=list(wrap.lines = TRUE, wrap.at=72)
                              )
    obj <-
    object <- new( "BaseDocumentation"
                 , author      = c( person('Andrew', 'Redd', email='andrew.redd@hsc.utah.edu')
                                  , person('Drew'  , 'Blue')
                                  )
                 , title       = 'Create function documentation'
                 , description = description
                 , seealso     = '\\link{documentation-package}'
                 , keywords    = 'internal'
                 , aliases     = 'test-alias'
                 , concepts    = c('test concept', 'testing', 'debugging')
                 , references  = citation()
                 )
    rd <- toRd(object)

    val <- stringi::stri_split_lines1(collapse0(rd))
    expected <- readLines(system.file("examples", "expected-output_toRd-Documentation.Rd", package="documentation"))
    expect_identical(val , expected)

    expect_equal(collapse0(rd[['\\author']]), "\\author{Andrew Redd \\email{andrew.redd@hsc.utah.edu} and Drew Blue}")
    expect_equal(collapse0(rd[['\\title']]), "\\title{Create function documentation}")
    expect_equal(collapse0(rd['\\keyword']), "\\keyword{internal}")
    expect_equal(collapse0(rd['\\alias']), "\\alias{test-alias}")
}





