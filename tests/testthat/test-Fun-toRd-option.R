#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Fun-toRd-option.R`')
#line 31 "R/Fun-toRd-option.R"
test_that('toRd,option-Documentation-method', { #@testing
    obj <- doc <- new('option-Documentation', 'anOption', 'a description')

    expect_identical(doc_get_name(doc), "anOption-option")
    expect_identical(doc_get_title(doc), "Documentation for Option 'anOption'")

    option.rd <- toRd(doc)

    expect_true(!anyDuplicated(names(option.rd)))

    expect_identical(option.rd[['name']], '\\name{anOption-option}')
    expect_identical(option.rd[['title']], "\\title{Documentation for Option 'anOption'}")
    expect_identical(option.rd[['description']], '\\description{a description}')


    # tmp2 <- tempfile('docs', fileext = '.Rd')
    # write_documentation(doc, fmt='Rd', file = textConnection('my_txt', 'w'))



    # expect_identical( my_txt
    #                 , c( "\\name{option-myOption)"
    #                    )
    #                 )

})
