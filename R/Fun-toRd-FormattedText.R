#' @include Classes.R
#' @include Fun-default.R

### toRd,Documentation-Keyword #####
#' @export
setMethod('toRd', 'Documentation-Keyword', function( obj, ...){
    if(length(obj) == 1 ) Rd_keyword(obj@.Data)
    cl(lapply(obj@.Data, Rd_keyword), 'Rd')
})
if(FALSE){#! @testing
    obj <- new('Documentation-Keyword', 'utilities')
    val <- toRd(obj)
    expect_is_exactly(val, 'Rd')
    expect_is_exactly(val[[1]], 'Rd_tag')
    expect_true(is_Rd_tag(val[[1]], '\\keyword'))

    obj <- new('Documentation-Keyword', c('utilities', 'character'))
    val <- toRd(obj)
    expect_is_exactly(val, 'Rd')
    expect_length(val, 2)
    expect_is_exactly(val[[1]], 'Rd_tag')
    expect_true(is_Rd_tag(val[[1]], '\\keyword'))
    expect_equal( collapse0(as.character(val))
                , '\\keyword{utilities}\\keyword{character}')
}


### toRd,FormattedText/Rd #####
#' @export
setMethod('toRd', 'FormattedText/Rd',
function( obj, ...){
    #! Convert formatted text into Rd lines.
    S3Part(obj, strictS3 =TRUE)
})
if(FALSE){#! @testing
    rd <- Rd( Rd_text("A description of ")
            , Rd_tag('\\code', Rd_tag('\\link', Rd_rcode("toRd")))
            , '\n'
            )
    obj <- FT_Rd(rd)
    class(rd) <- s('Rd', package='Rd')

    val <- toRd(obj)
    identical(S3Part(obj, strictS3 =TRUE), rd)
    identical(S3Part(obj, strictS3 =TRUE)[[1]], rd[[1]])
    identical(S3Part(obj, strictS3 =TRUE)[[2]], rd[[2]])
    identical(S3Part(obj, strictS3 =TRUE)[[3]], rd[[3]])

    expect_is(obj, 'FormattedText/Rd')
    val <- toRd(obj)
    expect_is_exactly(val, 'Rd')
    expect_identical( val, rd)

    obj <- FT_Rd('Hello world!')
    val <- toRd(obj)
    expect_is_exactly(val, 'Rd')
    attr(class(val), 'package') <- NULL
    expect_equal( val, Rd('Hello world!'))
    expect_false(identical(toRd(obj), obj))
}

### toRd,FormattedText/character #####
#' @export
setMethod('toRd', 'FormattedText/character',
function( obj, ..., control=list()){
    txt <- S3Part(obj, strictS3 =TRUE)
    if (length(txt)==0L) return(Rd())
    if (length(txt)==1L) return(toRd(txt))
    Rd(collapse(txt, '\n\n'))
})
if(FALSE){#@testing
    obj <- FormattedText(stringi::stri_rand_lipsum(3))
    as.rd <- toRd(obj)
    expect_length(as.rd, 5 )
    expect_length(as.character(as.rd), 5 )
    expect_is_exactly(as.rd, 'Rd')

    expect_true(all(as.rd[c(2,4)]=='\n'))

    expect_identical(toRd(FT()), Rd())
}
