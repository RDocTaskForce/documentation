#! This file was automatically produced by documentation::extract_tests on  2018-05-23 19:03:27
#! changes will be overwritten.
context('tests extracted from file `Fun-toRd-function.R`')
#line 15 "/rdtf/documentation/R/Fun-toRd-function.R"
test_that('toRd,function-Documentation-method', {#! @testing
    obj <- new( "function-Documentation"
              , name = as.name('function_documentation')
              , title = 'Create function documentation'
              , author = person('Andrew', 'Redd', email='andrew.redd@hsc.utah.edu')
              , usage= call('function_documentation', as.name('name'), as.name('arguments'), as.name('usage'), as.name('...'))
              , arguments = ArgumentList( arg(name     , "Name of the function")
                                        , arg(arguments, "Argument list"               , class="ArgumentList")
                                        , arg(value    , "Return value of the function")
                                        , arg(usage    , "Usage string to override default, constructed from the name and arguments.", class="call")
                                        , arg('...'    , "other arguments to contruct the Documentation obj.")
                                        )
              , value = "A function-Documentation obj."
              )
    Rd <- toRd(obj)
    expect_is(Rd, 'character')
    expect_true(all(c('name', 'usage', 'value', 'arguments') %in% names(Rd)))
    expect_equal(Rd[['name']], '\\name{function_documentation}')
    expect_equal(Rd[['value']], '\\value{A function-Documentation obj.}')
    expect_equal(Rd[['usage']], '\\usage{function_documentation(name, arguments, usage, ...)}')
    expect_equal(length(Rd[['arguments']]), 1)
})
