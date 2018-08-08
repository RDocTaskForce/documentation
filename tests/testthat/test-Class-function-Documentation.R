#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Class-function-Documentation.R`')
#line 43 "/rdtf/documentation/R/Class-function-Documentation.R"
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
#line 72 "/rdtf/documentation/R/Class-function-Documentation.R"
test_that('documentation<-,function,function-Documentation', {#@testing documentation<-,function,function-Documentation
    hw <- function(){print("hello world")}
    documentation(hw) <- function_documentation(title = "the standard Hello world")

    docs <- documentation(hw)
    expect_is(docs, 'function-Documentation')
    expect_identical(docs, function_documentation(title = "the standard Hello world"))
    expect_true(.is_undefined(docs@name))
    expect_identical(docs@title, "the standard Hello world")
})
