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
    c( Rd_name(doc_get_name(obj))
     , Rd_title(doc_get_title(obj))
     , Rd_description(toRd(doc_get_description(obj)))
     , Rd_aliases(doc_get_aliases(obj))
     )
})
if(FALSE){ #@testing
    obj <- doc <- new('option-Documentation', 'anOption', 'a description')

    expect_identical(doc_get_name(doc), "anOption-option")
    expect_identical(doc_get_title(doc), "Documentation for Option 'anOption'")

    option.rd <- toRd(doc)



    expect_identical(option.rd[['\\name']], Rd_name('anOption-option'))
    expect_identical(option.rd[['\\title']], Rd_title("Documentation for Option 'anOption'"))
    expect_identical(option.rd[['\\description']], Rd_description('a description'))
    expect_identical(option.rd['\\alias'], Rd(Rd_alias('anOption'), Rd_alias('anOption-option')))
}
