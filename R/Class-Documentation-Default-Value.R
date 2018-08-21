#' @include utils.R

setClass('Documentation-Default-Value'           , contains="VIRTUAL")
setClass('Documentation-Default-Value:logical'   , contains=c('Documentation-Default-Value', 'logical'   ))
setClass('Documentation-Default-Value:integer'   , contains=c('Documentation-Default-Value', 'integer'   ))
setClass('Documentation-Default-Value:numeric'   , contains=c('Documentation-Default-Value', 'numeric'   ))
setClass('Documentation-Default-Value:list'      , contains=c('Documentation-Default-Value', 'list'      ))
setClass('Documentation-Default-Value:expression', contains=c('Documentation-Default-Value', 'expression'))
setClass('Documentation-Default-Value:character' , contains=c('Documentation-Default-Value', 'character' ))
setClass('Documentation-Default-Value:formula'   , contains=c('Documentation-Default-Value', 'formula'   ))
setClass('Documentation-Default-Value:function'  , contains=c('Documentation-Default-Value', 'function'  ))
setClass('Documentation-Default-Value:name'      , contains=c('Documentation-Default-Value', 'name'      ))
setClass('Documentation-Default-Value:NULL'      , contains=c('Documentation-Default-Value', 'NULL', prototype=NULL))
setClass('Documentation-No-Default-Value'        , contains='Documentation-Default-Value' )
.no.default <- new('Documentation-No-Default-Value')
#' @export
`as.character.Documentation-No-Default-Value` <- function(x,...)''

setAs('logical'   , 'Documentation-Default-Value', function(from){new('Documentation-Default-Value:logical'   , from)})
setAs('numeric'   , 'Documentation-Default-Value', function(from){new('Documentation-Default-Value:numeric'   , from)})
setAs('list'      , 'Documentation-Default-Value', function(from){new('Documentation-Default-Value:list'      , from)})
setAs('expression', 'Documentation-Default-Value', function(from){new('Documentation-Default-Value:expression', from)})
setAs('character' , 'Documentation-Default-Value', function(from){new('Documentation-Default-Value:character' , from)})
setAs('function'  , 'Documentation-Default-Value', function(from){new('Documentation-Default-Value:function'  , from)})
setAs('name'      , 'Documentation-Default-Value', function(from){new('Documentation-Default-Value:name'      , from)})
setAs('NULL'      , 'Documentation-Default-Value', function(from){new('Documentation-Default-Value:NULL'      , from)})

setClass( 'Documentation-Default-Value:language'
        , contains  = c('Documentation-Default-Value', 'language')
        )
setMethod('initialize', 'Documentation-Default-Value:language',
    function(.Object, value, ...){
        stopifnot(is(value, 'language'))
        attr(value, ".S3Class") <- class(value)
        .Object <- methods:::.asS4(methods:::.mergeAttrs(value, .Object))
        .Object
    })


setAs('language', 'Documentation-Default-Value', function(from){new('Documentation-Default-Value:language'      , from)})


if(FALSE){#@testing Documentation-Default-Value
    expect_is(dv.lgl <- new('Documentation-Default-Value:logical'   , TRUE             ), 'Documentation-Default-Value:logical'   )
    expect_is(dv.dbl <- new('Documentation-Default-Value:numeric'   , 2.5              ), 'Documentation-Default-Value:numeric'   )
    expect_is(dv.lst <- new('Documentation-Default-Value:list'      , list(1,2,3)      ), 'Documentation-Default-Value:list'      )
    expect_is(dv.exp <- new('Documentation-Default-Value:expression', expression()     ), 'Documentation-Default-Value:expression')
    expect_is(dv.chr <- new('Documentation-Default-Value:character' , ""               ), 'Documentation-Default-Value:character' )
    expect_is(dv.fun <- new('Documentation-Default-Value:function'  , function(){}     ), 'Documentation-Default-Value:function'  )
    expect_is(dv.nam <- new('Documentation-Default-Value:name'      , as.name('test')  ), 'Documentation-Default-Value:name'      )
    expect_is(dv.nul <- new('Documentation-Default-Value:NULL'                         ), 'Documentation-Default-Value:NULL'      )
    expect_is(          new('Documentation-Default-Value:NULL'      , NULL             ), 'Documentation-Default-Value:NULL'      )

    expect_is( as(as.name('hello'), 'Documentation-Default-Value'), 'Documentation-Default-Value')
    expect_is( as(substitute(x+y) , 'Documentation-Default-Value'), 'Documentation-Default-Value')

    expect_error( dv <- new('Documentation-Default-Value')
                , "trying to generate an object from a virtual class"
                )

    expect_identical(as.character(.no.default), '')

    expect_identical( as(TRUE             , 'Documentation-Default-Value'), dv.lgl)
    expect_identical( as(2.5              , 'Documentation-Default-Value'), dv.dbl)
    expect_identical( as(list(1,2,3)      , 'Documentation-Default-Value'), dv.lst)
    expect_identical( as(expression()     , 'Documentation-Default-Value'), dv.exp)
    expect_identical( as(""               , 'Documentation-Default-Value'), dv.chr)
    expect_identical( as(function(){}     , 'Documentation-Default-Value'), dv.fun)
    expect_identical( as(as.name('test')  , 'Documentation-Default-Value'), dv.nam)
    expect_identical( as(NULL             , 'Documentation-Default-Value'), dv.nul)
}
