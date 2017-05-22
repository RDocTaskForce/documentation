#' @include Class-option-documentation.R
#' @include utils-backslash.R

default_ <- 
function( name
        , default.value = NULL
        , which = -1
        , fun   = as.character(sys.call(which))[[1L]]
        , where = topenv(environment(sys.function(which)))
        , pkg   = getPackageName(where)
        , pkg.name = name
        , search.global = TRUE
        , global.name = pkg.name
        ){
    #' Find the defaults that might be specified for the package.
    if(!is.null(pkg) && pkg =='.GlobalEnv') pkg <- NULL
    if(!is.null(pkg) && pkg == '') pkg <- NULL
    if(!is.null(fun) && fun == '') fun <- NULL
    value <- NULL
    if(is.null(value) && !is.null(pkg) && !is.null(fun))
        value <- getOption( paste('defaults', pkg,  fun,        name, sep='::'), NULL)
    if(is.null(value) && search.global && !is.null(fun))
        value <- getOption( paste('defaults',       fun, global.name, sep='::'), NULL)
    if(is.null(value) && !is.null(pkg))
        value <- getOption( paste('defaults', pkg,          pkg.name, sep='::'), NULL)
    if(is.null(value) && search.global)
        value <- getOption( paste('defaults',            global.name, sep='::'), NULL)
    if(is.null(value))
        value <- default.value
    return(value)
}
if(FALSE){#! @testing
    opar <- options()
    
    options( 'defaults::documentation::default_test_function::test.arg' = 1
           , 'defaults::documentation::inherited.arg' = 2
           , 'defaults::default_test_function::fun.global.arg' = 3
           , 'defaults::global.arg' = "abc"
           )

    expect_equal(default_('test.arg'      , TRUE, fun='default_test_function', pkg='documentation'), 1)
    expect_equal(default_('inherited.arg' , TRUE, fun='default_test_function', pkg='documentation'), 2)
    expect_equal(default_('fun.global.arg', TRUE, fun='default_test_function', pkg='documentation'), 3)
    expect_equal(default_('global.arg'    , TRUE, fun='default_test_function', pkg='documentation'), 'abc')
    expect_true (default_('no.arg'        , TRUE, fun='default_test_function', pkg='documentation'))
 
    options( 'defaults::default_test_function::test.arg' = 1
           , 'defaults::inherited.arg' = 2
           )

    default_test_function <- 
    function( which = .T(test, inherited, no)
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
    
    options(opar)
}

default <- 
function( arg 
        , default.value = NULL
        , ...
        , which = -1
        , fun   = as.character(sys.call(which))[[1L]]
        , where = topenv(environment(sys.function(which)))
        , pkg   = getPackageName(where)
        , search.global = FALSE
        ){
    name <- deparse(substitute(arg))
    
    force(fun)
    force(pkg)
    default_( name=name, default.value=default.value
            , fun=fun, pkg=pkg
            , ...
            )
}
if(FALSE){#! @testing
    opar <- options()
    options( 'defaults::documentation::default_test_function::test.arg' = 1
           , 'defaults::documentation::inherited.arg'                   = 2
           , 'defaults::default_test_function::fun.global.arg'          = 3
           , 'defaults::global.arg' = "abc"
           )
    expect_equal(default(test.arg      , TRUE, fun='default_test_function', pkg='documentation'), 1)
    expect_equal(default(inherited.arg , TRUE, fun='default_test_function', pkg='documentation'), 2)
    expect_equal(default(fun.global.arg, TRUE, fun='default_test_function', pkg='documentation'), 3)
    expect_equal(default(global.arg    , TRUE, fun='default_test_function', pkg='documentation'), 'abc')
    expect_true (default(no.arg        , TRUE, fun='default_test_function', pkg='documentation'))


    options( 'defaults::default_test_function::test.arg' = 1
           , 'defaults::inherited.arg' = 2
           , 'defaults::global.arg' = "abc"
           )

    default_test_function <- 
    function( which = .T(test, inherited, global, no)
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
    default_test_function('global')
    
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

set_option_documentation( "defaults::documentation::indent"
   , description = "Determines if code should be indented when formatted."
   )


#~ defaults$indent.with <- '    '

set_option_documentation( "defaults::documentation::indent.with"
   , description = "Determines what to indent with, when getOption('documentation::format_md::indent') is TRUE."
   )


#~ defaults$collapse.lines <- FALSE

set_option_documentation("defaults::documentation::collapse.lines"
   , description = "should documentation functions return a single " %\%
                   "string (TRUE) or a array of strings (FALSE) " %\%
                   "representing the lines of documentation."
   )

#~ defaults$collapse.with <- '\n'
set_option_documentation("defaults::documentation::collapse.with"
   , description = "when \\code{getOption(documentation::format_md::collapse.lines)}" %\%
                   "is \\code{TRUE} what the lines should be separated with."
   )

set_option_documentation("defaults::documentation::verbose"
   , description = "Should verbose message be given?"
   )
