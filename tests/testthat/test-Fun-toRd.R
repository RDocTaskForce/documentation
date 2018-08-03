#! This file was automatically produced by documentation::extract_tests on  2018-08-03 01:24:29
#! changes will be overwritten.
context('tests extracted from file `Fun-toRd.R`')
#line 38 "/rdtf/documentation/R/Fun-toRd.R"
test_that('Rd_tag', {#! @testing
    expect_error(Rd_tag('test', NULL), "Rd tag name must be a single character")
    expect_error(Rd_tag('test', c('a', 'b')), "Rd tag name must be a single character")
    expect_error(Rd_tag('test', 1), "Rd tag name must be a single character")
    expect_equal(Rd_tag('my name', 'name'), "\\name{my name}")
    expect_equal( Rd_tag(c('line1', 'line2'), 'name')
                , c('\\name{', 'line1', 'line2', '}')
                )
    name <- 'testing'
    expect_equal(Rd_tag(name), '\\name{testing}')
    
    obj <- new('FormattedText', stringi::stri_rand_lipsum(3))
    as.tag <- Rd_tag(obj)
    expect_is(as.tag, 'character')
    expect_length(as.tag, 7)
    expect_identical(as.tag[c(1,7)], c('\\obj{', '}'))
})
#line 66 "/rdtf/documentation/R/Fun-toRd.R"
test_that('toRd.person', {#! @testing
    object <- person('Andrew', 'Redd', email='andrew.redd@hsc.utah.edu')
    expect_equal(toRd(object), 'Andrew Redd \\email{andrew.redd@hsc.utah.edu}')
    
    object <-c( person('First' , 'Author', email='me1@email.com')
              , person('Second', 'Author', email='me2@email.com') 
              )
    expect_equal(toRd(object), c('First Author \\email{me1@email.com}'
                                , 'Second Author \\email{me2@email.com}'
                                ) )
    expect_equal(toRd( c( person('Andrew', 'Redd', email='andrew.redd@hsc.utah.edu')
                        , person('Drew'  , 'Blue')
                        ) )
                , c( 'Andrew Redd \\email{andrew.redd@hsc.utah.edu}'
                   , 'Drew Blue'
                   ) 
                )
               
        

})
#line 89 "/rdtf/documentation/R/Fun-toRd.R"
test_that('documentation bibstyle', {#!@testing documentation bibstyle
    object <- citation() %>% structure(class='bibentry')
    default.style <- toRd(object, style='JSS')
    doc.style     <- toRd(object, style='documentation')
    
    expect_true(default.style != doc.style)
})
#line 99 "/rdtf/documentation/R/Fun-toRd.R"
test_that('toRd,Documentation-Keyword-method', {#! @testing
    obj <- new('Documentation-Keyword', c('utilities', 'character'))
    expect_equal(toRd(obj), c('\\keyword{utilities}', '\\keyword{character}'))
})
#line 117 "/rdtf/documentation/R/Fun-toRd.R"
test_that('toRd,FormattedText-method', {#! @testing
    obj <- FormattedText()
    expect_identical(toRd(obj), character(0))
    
    obj <- FormattedText('Hello world!')
    expect_identical(toRd(obj), 'Hello world!')
    expect_false(identical(toRd(obj), obj))
    
    obj <- FormattedText(stringi::stri_rand_lipsum(3))
    as.rd <- toRd(obj)
    expect_equal(length(as.rd), 5 )
    expect_is(as.rd, 'character')
})
#line 138 "/rdtf/documentation/R/Fun-toRd.R"
test_that('toRd.vector', {#@testing
    obj <- new('FormattedText', stringi::stri_rand_lipsum(3))
    expect_is(obj, 'vector')
    as.rd <- toRd(obj, 'description')
    
})
