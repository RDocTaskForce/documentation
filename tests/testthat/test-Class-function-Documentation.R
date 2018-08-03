#! This file was automatically produced by documentation::extract_tests on  2018-08-03 01:24:28
#! changes will be overwritten.
context('tests extracted from file `Class-function-Documentation.R`')
#line 42 "/rdtf/documentation/R/Class-function-Documentation.R"
test_that('initialize,function-Documentation-method', {#! @testing
    empty.object <- new( "function-Documentation")
    expect_is(empty.object, "function-Documentation")
    
    named.object <- new("function-Documentation", name = "Heisenburg")
    expect_is(named.object,"function-Documentation")
    expect_equal(deparse(getElement(named.object, 'name')), "Heisenburg")
    
    named.object <- new("function-Documentation", name = as.name("Heisenburg"))
    
    
    object <- new( "function-Documentation"
                 , name = as.name('function_documentation')
                 , title = 'Create function documentation'
                 , author = person('Andrew', 'Redd', email='andrew.redd@hsc.utah.edu')
                 , usage= call('function_documentation', as.name('name'), as.name('arguments'), as.name('usage'), as.name('...'))
                 , arguments = ArgumentList( arg(name     , "Name of the function")
                                           , arg(arguments, "Argument list"               , class="ArgumentList")
                                           , arg(value    , "Return value of the function")
                                           , arg(usage    , "Usage string to override default, constructed from the name and arguments.", class="call")
                                           , arg('...'    , "other arguments to contruct the Documentation object.")
                                           )
                 , description = "create documentation for a function"
                 , value = "A function-Documentation object."
                 )
    object <- function_documentation()
    expect_equal(deparse(object@name), "<UNDEFINED>")
})
#line 84 "/rdtf/documentation/R/Class-function-Documentation.R"
test_that('documentation<-,function,function-Documentation-method', {#@testing
    # trace("documentation<-", signature = c('function', 'function-Documentation'), browser)
    hw <- function(){print("hello world")}
    documentation(hw) <- function_documentation(title = "the standard Hello world")
    
    
})
