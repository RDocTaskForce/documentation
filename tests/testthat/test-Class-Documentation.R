#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Class-Documentation.R`')
#line 77 "R/Class-Documentation.R"
test_that('initialize,BaseDocumentation-method', {#!@testing
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
})
#line 130 "R/Class-Documentation.R"
test_that('setGeneric("documented", ...)', {#@testing
    object <- function(msg="hello world"){print(msg)}
    dobj <- documented(object, name='object', title="hello world example")

    expect_false(is.null(attr(dobj, 'documentation')))
    expect_is(attr(dobj, 'documentation'), 'function-Documentation')

})
#line 146 "R/Class-Documentation.R"
test_that('as.list,Documentation-method', {#! @testing
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
})
