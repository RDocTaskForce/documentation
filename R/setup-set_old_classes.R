
#' @importFrom utils bibentry person
setOldClass('bibentry')
setOldClass('person')

# from devtools & roxygen
setOldClass(c("package", 'list'))
setOldClass('roxy_block')

# from htmltools
setOldClass(c("shiny.tag", 'list'))

setOldClass(c('Rd', 'character'))
if(FALSE){
    x <- cl('text', 'Rd')
    expect_is(x, 'Rd')
    
    
    
    
}