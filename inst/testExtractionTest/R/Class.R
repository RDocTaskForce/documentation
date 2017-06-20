# A test cases for extracting tests from a package structure.

setClass("Test-Class")
if(FALSE){#!@test
    expect_true(TRUE)
    expect_is(getClass("Test-Class"), "classRepresentation")
}

setMethod("show", "Test-Class", function(object){cat("hi")})
if(FALSE){#!@test
    expect_true(TRUE)
}

setGeneric("yolo", function(object, ...){cat("you only live once!")})
if(FALSE){#!@test
    expect_true(TRUE)
}
