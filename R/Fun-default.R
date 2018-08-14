#' @include Classes.R

default_ <-
function( name
        , default.value = NULL
        , n = 1L
        , fun
        , where
        , pkg
        ){
    #' Find the defaults that might be specified for the package.
    if (missing(fun) || missing(pkg)){
        call.no <- min(which(sapply(sys.frames(), identical, parent.frame(n))))
        if (missing(fun)) fun <- as.character(sys.call(call.no))[[1L]]
        if (missing(where)) where <- topenv(parent.frame(n))
        pkg   = getPackageName(where)
    }


    if (!is.null(pkg))
        if(pkg =='.GlobalEnv' || pkg == "base" || pkg == '') pkg <- NULL
    if (!is.null(fun))
        if(fun == '') fun <- NULL

    if (!is.null(pkg) && !is.null(fun)
      &&!is.null(value <- getOption(paste(pkg, fun, name, sep='::'))))
        return(value)
    if (!is.null(pkg)
      &&!is.null(value <- getOption(paste(pkg, name, sep='::'))))
        return(value)
    if (!is.null(fun)
      &&!is.null(value <- getOption(paste(fun, name, sep='::'))))
        return(value)
    if (!is.null(value <- getOption(name)))
        return(value)
    return(default.value)
}
if(FALSE){#! @testing
    o <-list( 'documentation::default_test_function::test.arg' = 1
            , 'documentation::inherited.arg' = 2
            , 'default_test_function::fun.global.arg' = 3
            , 'global.arg' = "abc"
            )

    withr::with_options(o, {
        expect_equal(default_('test.arg'      , TRUE, fun='default_test_function', pkg='documentation'), 1    )
        expect_equal(default_('inherited.arg' , TRUE, fun='default_test_function', pkg='documentation'), 2    )
        expect_equal(default_('fun.global.arg', TRUE, fun='default_test_function', pkg='documentation'), 3    )
        expect_equal(default_('global.arg'    , TRUE, fun='default_test_function', pkg='documentation'), 'abc')
        expect_true (default_('no.arg'        , TRUE, fun='default_test_function', pkg='documentation')       )
    })

    withr::with_options(list( 'default_test_function::test.arg' = 1
                            , 'inherited.arg' = 2
                            ), {
    default_test_function <-
    function( which = c('test', 'inherited', 'no')
            , test.arg      = default_('test.arg'      , FALSE, pkg=NULL)
            , inherited.arg = default_('inherited.arg' , FALSE, pkg=NULL)
            , no.arg        = default_('no.arg'        , FALSE, pkg=NULL)
            ){
        switch( match.arg(which)
              , test      = test.arg
              , inherited = inherited.arg
              , no        = no.arg
              )
    }
    default_test_function('test')

    expect_equal(default_test_function('test'), 1)
    expect_equal(default_test_function('inherited'), 2)
    expect_equal(default_test_function('no'), FALSE)
    })

}

default <-
function( arg
        , default.value = NULL
        , n = 1
        , ...
        ){
    name <- deparse(substitute(arg))
    default_( name=name, default.value=default.value
            , n=n+1L
            , ...
            )
}
if(FALSE){#! @testing
    opar <- options()
    options( 'documentation::default_test_function::test.arg' = 1
           , 'documentation::inherited.arg'                   = 2
           , 'default_test_function::fun.global.arg'          = 3
           , 'global.arg' = "abc"
           )
    expect_equal(default(test.arg      , TRUE, fun='default_test_function', pkg='documentation'), 1)
    expect_equal(default(inherited.arg , TRUE, fun='default_test_function', pkg='documentation'), 2)
    expect_equal(default(fun.global.arg, TRUE, fun='default_test_function', pkg='documentation'), 3)
    expect_equal(default(global.arg    , TRUE, fun='default_test_function', pkg='documentation'), 'abc')
    expect_true (default(no.arg        , TRUE, fun='default_test_function', pkg='documentation'))


    options( 'default_test_function::test.arg' = 1
           , 'inherited.arg' = 2
           , 'global.arg' = "abc"
           )

    default_test_function <-
    function( which = c('test', 'inherited', 'global', 'no')
            , test.arg      = default(test.arg      , FALSE, pkg=NULL)
            , inherited.arg = default(inherited.arg , FALSE, pkg=NULL)
            , global.arg    = default(global.arg    , FALSE, pkg=NULL)
            , no.arg        = default(no.arg        , FALSE, pkg=NULL)
            ){
        switch( match.arg(which)
              , test      = test.arg
              , inherited = inherited.arg
              , global    = global.arg
              , no        = no.arg
              )
    }

    expect_equal(default_test_function('test'), 1)
    expect_equal(default_test_function('inherited'), 2)
    expect_equal(default_test_function('global'), 'abc')
    expect_equal(default_test_function('no'), FALSE)
    options(opar)
}

if(FALSE){
    undebug(default)
    debug(default)

    options(test_function)
}


#~ defaults$indent <- FALSE

# set_option_documentation( "defaults::documentation::indent"
#    , description = "Determines if code should be indented when formatted."
#    )


#~ defaults$indent.with <- '    '

# set_option_documentation( "defaults::documentation::indent.with"
#    , description = "Determines what to indent with, when getOption('defaults::documentation::indent') is TRUE."
#    )


#~ defaults$collapse.lines <- FALSE

# set_option_documentation("defaults::documentation::collapse.lines"
#    , description = "should documentation functions return a single " %<<%
#                    "string (TRUE) or a array of strings (FALSE) " %<<%
#                    "representing the lines of documentation."
#    )

#~ defaults$collapse.with <- '\n'
# set_option_documentation("defaults::documentation::collapse.with"
#    , description = "when \\code{getOption('defaults::documentation::collapse.lines\')}" %<<%
#                    "is \\code{TRUE} what the lines should be separated with."
#    )

# set_option_documentation("defaults::documentation::verbose"
#    , description = "Should verbose message be given?"
#    )
