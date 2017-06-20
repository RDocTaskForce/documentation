#! This file was automatically produced by documentation::extract_tests on  2017-06-20 13:15:39
#! changes will be overwritten.
context('tests extracted from file `/mnt/data/projects/rdtf/documentation/R/Fun-toRd.R`')
#line 35 "/mnt/data/projects/rdtf/documentation/R/Fun-toRd.R"
test_that('Rd_tag', {#! @testing
    expect_equal(Rd_tag('my name', 'name'), "\\name{my name}")
    expect_equal(Rd_tag('test', NULL), character(0))
    expect_equal( Rd_tag(c('line1', 'line2'), 'name')
                , c('\\name{', 'line1', 'line2', '}')
                )
    name <- 'testing'
    expect_equal(Rd_tag(name), '\\name{testing}')
})
#line 55 "/mnt/data/projects/rdtf/documentation/R/Fun-toRd.R"
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
#line 78 "/mnt/data/projects/rdtf/documentation/R/Fun-toRd.R"
test_that('documentation bibstyle', {#!@testing documentation bibstyle
    object <- citation() %>% structure(class='bibentry')
    default.style <- toRd(object, style='JSS')
    doc.style     <- toRd(object, style='documentation')
    
    expect_true(default.style != doc.style)
})
#line 88 "/mnt/data/projects/rdtf/documentation/R/Fun-toRd.R"
test_that('toRd.Documentation-Keyword', {#! @testing
    obj <- new('Documentation-Keyword', c('utilities', 'character'))
    expect_equal(toRd(obj), c('\\keyword{utilities}', '\\keyword{character}'))
})
#line 106 "/mnt/data/projects/rdtf/documentation/R/Fun-toRd.R"
test_that('toRd.FormattedText', {#! @testing
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
