#' @import htmltools

#TODO
#' only text stored in an HTML5/XHTML compliant format.
setClass( "FormattedText", contains = 'character')



if(FALSE){
    as("hello world", 'FormattedText')
    
    
    using(xml2, htmltools)
    
    x <- read_html("<div attr=\"value\">regular text<a>link text</a></div>")
    
    y <- div('Regular', a('link'))
    class(y)
    
    
    
}
