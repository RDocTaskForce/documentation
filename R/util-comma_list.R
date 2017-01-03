comma_list <- 
function( x                 #< vector to make into a comma list
        , sep  = ", "       #< separator for multiple elements.
        , sep2 = " and "    #< separator for only two elements.
        , sep.last = ", and " #< separator between last and second to last for more than two elements.
        , terminator = ''   #< ends the list.   
        ){
    #! Create a properly formatted comma separated list.
    if (length(x) == 1) return(paste(x))
    else if (length(x) == 2)
        return (paste(x, collapse=sep2))
    else
        return(paste(x, c(rep(sep, length(x)-2), sep.last, terminator), sep='', collapse=''))
}
if(FALSE){#! @testing
    expect_is(comma_list(1), 'character')
    expect_equal(comma_list(1), '1')
    
    expect_equal(comma_list(1:2), '1 and 2')
    
    expect_equal(comma_list(1:3), '1, 2, and 3')
}
