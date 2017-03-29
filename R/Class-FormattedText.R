#TODO
#' only text stored in an HTML5/XHTML compliant format.
FormattedText <- 
setClass( "FormattedText", contains = 'character')
#TODO: needs validation for valid HTML5 text.


if(FALSE){
    as("hello world", 'FormattedText')
    
    
    using(xml2, htmltools)
    
    x <- read_html("<div attr=\"value\">regular text<a>link text</a></div>")
    
    y <- div('Regular', a('link'))
    class(y)
    
    
    
}
