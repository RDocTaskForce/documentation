# A test cases for extracting tests from a package structure.

hello_world <- function(){
    print("hello world")
}
if(FALSE){#!@testthat
    expect_output(hello_world(), "hello world")
}
