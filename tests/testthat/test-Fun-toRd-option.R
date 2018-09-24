#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Fun-toRd-option.R`')
#line 31 "R/Fun-toRd-option.R"
test_that('toRd,option-Documentation-method', { #@testing
    obj <- doc <- new('option-Documentation', 'anOption', 'a description')

    expect_identical(doc_get_name(doc), "anOption-option")
    expect_identical(doc_get_title(doc), "Documentation for Option 'anOption'")

    option.rd <- toRd(doc)



    expect_identical(option.rd[['\\name']], Rd_name('anOption-option'))
    expect_identical(option.rd[['\\title']], Rd_title("Documentation for Option 'anOption'"))
    expect_identical(option.rd[['\\description']], Rd_description('a description'))
    expect_identical(option.rd['\\alias'], Rd(Rd_alias('anOption'), Rd_alias('anOption-option')))
})
