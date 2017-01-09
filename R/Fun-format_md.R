


setGeneric('format_md', 
    function(object, ...){
        if(isS4(object)){
            slot.names <- slotNames(object)
            slots      <- lapply(slot.names, slot, object=object)
            formatted  <- sapply(slots, format_md)
            interleave( sprintf("@%s:", names)
                      , formatted
                      )
        } else if(is.list(object)) {
            interleave( sprintf("$%s:", names(object))
                      , lapply(object, format_md)
                      )
        } else {
            format(object, ...)
        }
    })

if(FALSE){#! @Testing
    expect_equal(format_md(1), '1')
    expect_equal(format_md('a'), 'a')
    
    format_md(list(a=1, b=TRUE, c='test'))
    
    object <- 
        setClass( "test-class"
                , slots=c(a='numeric', b='logical', c='character')
                )(a=1, b=TRUE, c='test')
    slotNames(object)
    getSlots(object)
    
    
}




