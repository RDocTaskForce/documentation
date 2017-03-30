#' @include Class-option-documentation.R
#' @include utils-backslash.R

default <- 
function( arg 
        , default.value
        , which = -1
        , fun   = as.character(sys.call(which))[[1L]]
        , where = topenv(environment(sys.function(which)))
        , pkg   = getPackageName(where)
        ){
    if(pkg =='.GlobalEnv') pkg <- NULL
            
    getOption( paste(c('defaults', pkg,  fun, deparse(substitute(arg))), collapse='::'),
    getOption( paste(c('defaults', pkg, NULL, deparse(substitute(arg))), collapse='::'), default.value))
}
if(FALSE){
    
    options( 'defaults::documentation::default_test_function::test.arg' = 1
           , 'defaults::documentation::inherited.arg' = 2
           )
    expect_equal(default(test.arg     , TRUE, fun='test_function', pkg='documentation'), 1)
    expect_equal(default(inherited.arg, TRUE, fun='test_function', pkg='documentation'), 2)
    expect_true (default(no.arg       , TRUE, fun='test_function', pkg='documentation'))


    options( 'defaults::default_test_function::test.arg' = 1
           , 'defaults::inherited.arg' = 2
           )

    default_test_function <- 
    function( which = .T(test, inherited, no)
            , test.arg      = default(test.arg      , FALSE)
            , inherited.arg = default(inherited.arg , FALSE)
            , no.arg        = default(no.arg        , FALSE)
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
