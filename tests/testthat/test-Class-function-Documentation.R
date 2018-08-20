#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Class-function-Documentation.R`')
#line 45 "R/Class-function-Documentation.R"
test_that('initialize,function-Documentation-method', {#! @testing
    empty.object <- new( "function-Documentation")
    expect_is(empty.object, "function-Documentation")

    named.object <- new("function-Documentation", name = "Heisenburg")
    expect_is(named.object,"function-Documentation")
    expect_equal(deparse(getElement(named.object, 'name')), "Heisenburg")

    fun.args <- ArgumentList( arg(name     , "Name of the function")
                            , arg(arguments, "Argument list"               , class="ArgumentList")
                            , arg(value    , "Return value of the function")
                            , arg(usage    , "Usage string to override default, constructed from the name and arguments.", class="call")
                            , arg('...'    , "other arguments to contruct the Documentation object.")
                            )
    object <- new( "function-Documentation"
                 , name = as.name('function_documentation')
                 , title = 'Create function documentation'
                 , author = person('Andrew', 'Redd', email='andrew.redd@hsc.utah.edu')
                 , usage= call('function_documentation', as.name('name'), as.name('arguments'), as.name('usage'), as.name('...'))
                 , arguments = fun.args
                 , description = "create documentation for a function"
                 , value = "A function-Documentation object."
                 )
    expect_is(object, 'Documentation')
    expect_is(object, 'BaseDocumentation')
    expect_is(object, 'function-Documentation')

    expect_identical(object@name,as.name('function_documentation'))
    expect_identical(object@title, 'Create function documentation')
    expect_identical(object@author, person('Andrew', 'Redd', email='andrew.redd@hsc.utah.edu'))
    expect_identical(object@usage, as(expression(function_documentation(name, arguments, usage, ...)), 'usage'))
    expect_identical(object@arguments, fun.args)
    expect_identical(object@description, FT("create documentation for a function"))
    expect_identical(object@value, FT("A function-Documentation object."))
        
    object <- function_documentation()
    expect_true(.is_undefined(object@name))
})
#line 84 "R/Class-function-Documentation.R"
test_that('documentation<-,function,function-Documentation', {#@testing documentation<-,function,function-Documentation
    hw <- function(){print("hello world")}
    documentation(hw) <- function_documentation(title = "the standard Hello world")

    docs <- documentation(hw)
    expect_is(docs, 'function-Documentation')
    expect_identical(docs, function_documentation(title = "the standard Hello world"))
    expect_true(.is_undefined(docs@name))
    expect_identical(docs@title, "the standard Hello world")
})
