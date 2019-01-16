#' @include Classes.R
#' @include Fun-accessors.R


#' @export
setMethod("doc_get_name", 'option-Documentation', function(doc){
    doc@key %<<<% "-option"
})
#' @export
setMethod("doc_get_title", 'option-Documentation', function(doc){
    ._("Documentation for Option '%s'", doc@key)
})
#' @export
setMethod("doc_get_details", 'option-Documentation', function(doc){
    if (inherits(doc@default, "Documentation-No-Default-Value"))
        ._("Option '%s' has no set default value.", doc@key)
    else if (inherits(doc@default, "Documenation-Default-Value"))
        ._("Option '%s' has default value of %s.", doc@key, doc@default)
    #TODO add formatting for including constraints in details.

})
#' @export
setMethod("doc_get_aliases", 'option-Documentation', function(doc){
    c(doc@key, doc_get_name(doc))
})

#' @export
setMethod("toRd", 'option-Documentation', function(obj, ..., raw.list=FALSE){
    l <- list( name        = Rd(Rd_name(doc_get_name(obj)))
             , aliases     = Rd(Rd_aliases(doc_get_aliases(obj)))
             , title       = Rd(Rd_title(doc_get_title(obj)))
             , description = Rd(Rd_description(toRd(doc_get_description(obj))))
             )
    if (raw.list) return(l)
    Rd_lines(l)
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

    expect_rd_output(option.rd, "Fun-toRd-option.Rd", 'toRd,option-Documenation')
}
