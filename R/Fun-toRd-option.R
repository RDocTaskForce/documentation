#' @include Classes.R
#' @include Fun-toRd.R
#' @include Fun-accessors.R


setMethod("doc_get_name", 'option-Documentation', function(doc){
    doc@key %<<<% "-option"
})
setMethod("doc_get_title", 'option-Documentation', function(doc){
    ._("Documentation for Option '%s'", doc@key)
})
setMethod("doc_get_details", 'option-Documentation', function(doc){
    if (inherits(doc@default, "Documentation-No-Default-Value"))
        ._("Option '%s' has no set default value.", doc@key)
    else if (inherits(doc@default, "Documenation-Default-Value"))
        ._("Option '%s' has default value of %s.", doc@key, doc@default)
    #TODO add formatting for including constraints in details.

})
setMethod("doc_get_aliases", 'option-Documentation', function(doc){
    c(doc@key, doc_get_name(doc))
})

setMethod("toRd", 'option-Documentation', function(obj){
    c( name  = Rd_tag(doc_get_name(obj), 'name')
     , title = Rd_tag(doc_get_title(obj), 'title')
     , description =  Rd_tag(doc_get_description(obj), 'description')
     , aliases = purrr::map_chr(doc_get_aliases(obj), Rd_tag, 'alias')
     )
})
if(FALSE){ #@testing
    obj <- doc <- new('option-Documentation', 'anOption', 'a description')

    expect_identical(doc_get_name(doc), "anOption-option")
    expect_identical(doc_get_title(doc), "Documentation for Option 'anOption'")

    option.rd <- toRd(doc)

    expect_true(!anyDuplicated(names(option.rd)))

    expect_identical(option.rd[['name']], '\\name{anOption-option}')
    expect_identical(option.rd[['title']], "\\title{Documentation for Option 'anOption'}")
    expect_identical(option.rd[['description']], '\\description{a description}')


    # tmp2 <- tempfile('docs', fileext = '.Rd')
    # write_documentation(doc, fmt='Rd', file = textConnection('my_txt', 'w'))



    # expect_identical( my_txt
    #                 , c( "\\name{option-myOption)"
    #                    )
    #                 )

}
