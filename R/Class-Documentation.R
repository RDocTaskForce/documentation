#' @include utils.R
#' @include Class-Vector.R
#' @include Class-Documentation-Keyword.R
#' @include Class-FormattedText.R
#' @include Class-example.R
#' @importFrom utils person
#' @import methods


#' @export
setClass('Documentation', contains='VIRTUAL')

### BaseDocumentation-class #####
#' @export
setClass( "BaseDocumentation", contains='Documentation'
        , slots = c( author      = "person"
                   , title       = "character"
                   , description = "FormattedText"
                   , references  = "bibentry"
                   , seealso     = "FormattedText"
                   , examples    = "Documentation-Examples"
                   , keywords    = "Documentation-Keyword"
                   , aliases     = "character"
                   , concepts    = "character"
                   , sections    = "SectionList"
                   , export      = "logical"     #< NA means defer decision.
                   )
        , prototype = list( author     = person()
                          , title = character(0)
                          , references = bibentry()
                          , seealso    = FT()
                          , description= FT()
                          , export=FALSE
                          )
        )

### intialize,BaseDocumentation #####
#' @export
setMethod("initialize", 'BaseDocumentation',
    function( .Object
            , ...
            , author      = NULL
            , description = NULL
            , keywords    = NULL
            , seealso     = NULL
            , examples    = NULL
            , references  = NULL
            )
    {
        .Object <- callNextMethod( .Object, ...)
        if (!is.null(keywords   ))  .Object@keywords    <- new("Documentation-Keyword", keywords   )
        if (!is.null(description))  .Object@description <- FT(description)
        if (!is.null(seealso    ))  .Object@seealso     <- FT(seealso    )
        if (!is.null(author     ))  .Object@author      <- utils::as.person(author)
        if (!is.null(examples   ))  .Object@examples    <- as(examples, 'Documentation-Examples')
        if (!is.null(references ))
            if(inherits(references, 'citation')){
                .Object@references <- structure(references, class='bibentry')
            } else if(inherits(references, 'bibentry')){
                .Object@references <- references
            } else {
                doc_error(._("Invalid argument for references, received %s." %<<%
                             "Expected a %s"
                            , dQuote(collapse(class(references), '/'))
                            , dQuote(getSlots(class(.Object))[['references']])
                            )
                         , type="invalid_argument")
            }
        return(.Object)
    })
if(FALSE){#!@testing
    x <- new('BaseDocumentation')
    expect_identical(x@author, person())
    expect_identical(x@title, character(0))
    expect_identical(x@description, FT(character(0)))
    expect_identical(x@seealso, FT(character(0)))
    expect_identical(x@keywords, new("Documentation-Keyword"))
    expect_identical(x@examples, new('Documentation-Examples'))
    expect_identical(x@references, cl(list(), 'bibentry'))

    x <- new('BaseDocumentation'
            , author     = person('Andrew', 'Redd')
            , references = citation()
            , title = "Test Documentation"
            , description = "plain text"
            , keywords = 'documentation'
            , seealso = Rd("\\code{\\link[function-Documentation-class]{function-documentation}}")
            , examples = expression(function_documentation("hw", title="Hello world"))
            )
    expect_identical( x@author, person('Andrew', 'Redd'))
    expect_identical( x@title,  "Test Documentation")
    expect_identical( x@description,  FT("plain text"))
    expect_identical( x@description,  FT("plain text"))
    expect_identical( x@keywords,  keyword("documentation"))
    expect_identical( x@seealso,  FT(Rd("\\code{\\link[function-Documentation-class]{function-documentation}}")))
    expect_identical( x@examples,  as(expression(function_documentation("hw", title="Hello world"))
                                     , 'Documentation-Examples'))
    expect_identical( cl(x@references, 'citation'), citation())

    cit <- citation()
    bib <- s(cit, class='bibentry')
    lrf <- list(bib)
    x <- new( 'BaseDocumentation', references = cit)
    expect_identical(doc_get_references(x), bib)

    x <- new( 'BaseDocumentation', references = bib)
    expect_identical(doc_get_references(x), bib)

    expect_error(x <- new( 'BaseDocumentation', references = lrf)
                , class = 'documentation-error-invalid_argument')
}

### documented #####
#' @export
setGeneric("documented",
function(object, ...){
    return(
        structure( object
                 , documentation = new(paste0(class(object)[[1]], '-Documentation') , ...)
                 )
          )

})
if(FALSE){#@testing
    object <- function(msg="hello world"){print(msg)}
    dobj <- documented(object, name='object', title="hello world example")

    expect_false(is.null(attr(dobj, 'documentation')))
    expect_is(attr(dobj, 'documentation'), 'function-Documentation')

}

### as.list,Documentation #####
#' @export
setMethod("as.list", 'Documentation',
function(x, ...){
    structure( lapply(slotNames(x), getElement, object=x)
             , names = slotNames(x)
             )
})
if(FALSE){#! @testing
    x <-
    object <- new( "BaseDocumentation"
                 , author      = c( person('Andrew', 'Redd', email='andrew.redd@hsc.utah.edu')
                                  , person('Drew'  , 'Blue')
                                  )
                 , title       = 'Create function documentation'
                 , description = stringi::stri_rand_lipsum(3)
                 , seealso     = '\\link{documentation-package}'
                 , keywords    = 'internal'
                 , aliases     = 'test-alias'
                 , references  = citation()
                 )
    object.as.list <- as.list(object)
    expect_is(object.as.list, 'list')
    expect_equal(names(object.as.list), slotNames(object))
}

