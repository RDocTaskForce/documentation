#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Fun-toRd-function.R`')
#line 34 "R/Fun-toRd-function.R"
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
    rd <- toRd(obj)
    expect_is_exactly(rd, 'Rd')

    expect_equal(rd[['\\name']], Rd_name('function_documentation'))
    expect_equal(rd[['\\value']], Rd_tag('value', Rd_text('A function-Documentation obj.')))
    expect_equal(rd[['\\usage']], Rd_usage(Rd_rcode('function_documentation(name, arguments, usage, ...)')))
    expect_equal(rd[['\\arguments']]
                , Rd_arguments( Rd_item('name', "Name of the function")
                              , Rd_item('arguments', "Argument list")
                              , Rd_item('value'    , "Return value of the function")
                              , Rd_item('usage'    , "Usage string to override default, constructed from the name and arguments.")
                              , Rd_item('...'    , "other arguments to contruct the Documentation obj.")
                              ))
    expect_false(any(nchar(rd) == 0))
})
