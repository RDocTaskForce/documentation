#' @include Classes.R
#' @include Fun-accessors.R

#' @export
setMethod("doc_get_name", 'option-Documentation', function(doc){
    "option-" %<<<% doc@key
})
if(FALSE){
    doc <- o <- new('option-Documentation', 'anOption', 'a description')
    expect_identical(doc_get_name(o), "anOption-option")
}
