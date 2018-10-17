#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Class-NamespaceEntry.R`')
#line 34 "/rdtf/documentation/R/Class-NamespaceEntry.R"
test_that('format.Export', {#@testing
    e <- export('my_name')
    expect_is(e, 'Export')
    expect_identical(e@name, 'my_name')

    expect_error(export())
    expect_error(export(NA_character_))
    expect_error(export(1))
    expect_error(export(TRUE))
    expect_error(export(letters))

    expect_identical( format(export("%<<%"))
                    , 'export("%<<%")')
})
#line 69 "/rdtf/documentation/R/Class-NamespaceEntry.R"
test_that('format.ExportPattern', {#@testing
    val <- export_pattern("^[^\\.]")
    expect_is(val, 'NamespaceEntry')
    expect_is(val, 'ExportEntry')
    expect_is(val, 'ExportPattern')
    expect_valid(val)

    expect_error(export_pattern("^[^\\."))

    val2 <- as("^[^\\.]", "ExportPattern")
    expect_identical(val, val2)

    expect_identical(format(val), 'exportPattern("^[^\\.]")')
})
#line 114 "/rdtf/documentation/R/Class-NamespaceEntry.R"
test_that('Class: ExportS3method', {#@testing Class: ExportS3method
    expect_error(export_s3method())
    expect_error(export_s3method('c'))
    val <- export_s3method('c', 'Rd')
    expect_is(val, 'ExportS3method')
    expect_equal(val@generic, 'c')
    expect_equal(val@signature, 'Rd')
    expect_equal(val@method, character(0))

    val <- export_s3method('print', 'class', 'my_special_printer')
    expect_equal(val@generic, 'print')
    expect_equal(val@signature, 'class')
    expect_equal(val@method, 'my_special_printer')

    expect_identical( format(export_s3method('print', 'class'))
                    , "S3method(print,class)" )
    expect_identical( format(export_s3method('print', 'class', "my print function"))
                    , "S3method(print,class,\"my print function\")" )
    expect_identical( format(export_s3method('[', 'my class'))
                    , 'S3method("[","my class")' )
})
#line 153 "/rdtf/documentation/R/Class-NamespaceEntry.R"
test_that('Class: ExportS4methods', {#@testing Class: ExportS4methods
    expect_error(export_s4methods())
    expect_error(export_s4methods(''))

    val <- export_s4methods('doc_get_name')
    expect_is(val, 'ExportS4methods')
    expect_equal(val@generic, 'doc_get_name')
    expect_valid(val)

    val <- export_s4methods('[[')
    expect_identical(format(val), 'exportMethods("[[")')
})
#line 183 "/rdtf/documentation/R/Class-NamespaceEntry.R"
test_that('Class: ExportS4class', {#@testing Class: ExportS4class
    expect_error(export_class())
    expect_error(export_class(''))
    expect_error(export_class(NA))

    val <- export_class('Documentation')
    expect_is(val, 'ExportS4class')
    expect_equal(val@name, 'Documentation')

    expect_identical(format(val), "exportClasses(Documentation)")
})
#line 216 "/rdtf/documentation/R/Class-NamespaceEntry.R"
test_that('Class: Import', {#@testing Class: Import
    expect_error(import())
    expect_error(import(''))

    val <- import('parsetools')
    expect_is(val, 'Import')
    expect_identical(val@package, 'parsetools')

    expect_identical(format(val), 'import(parsetools)')
})
#line 251 "/rdtf/documentation/R/Class-NamespaceEntry.R"
test_that('Class: ImportFrom', {#@testing Class: ImportFrom
    expect_error(import_from())
    expect_error(import_from('pkg'))
    expect_error(import_from('pkg', ''))
    expect_error(import_from('pkg', c('a', NA)))

    val <- import_from('pkg', 'name')
    expect_is(val, 'ImportFrom')
    expect_equal(val@package, 'pkg')
    expect_equal(val@names, 'name')

    val <- import_from('pkg', c('a', 'b'))
    expect_is(val, 'ImportFrom')
    expect_equal(val@package, 'pkg')
    expect_equal(val@names, c('a', 'b'))

    expect_identical( format(val)
                    , c("importFrom(pkg,a)", "importFrom(pkg,b)"))
})
#line 296 "/rdtf/documentation/R/Class-NamespaceEntry.R"
test_that('Class: ImportClassesFrom', {#@testing Class: ImportClassesFrom
    expect_error(import_classes_from())
    expect_error(import_classes_from('pkg'))
    expect_error(import_classes_from('pkg', ''))

    val <- import_classes_from('pkg', 'my_class')
    expect_is(val, 'ImportClassesFrom')
    expect_equal(val@package, 'pkg')
    expect_equal(val@names, 'my_class')

    val <- import_classes_from('pkg', c('class1', 'class2'))
    expect_is(val, 'ImportClassesFrom')
    expect_equal(val@package, 'pkg')
    expect_equal(val@names, c('class1', 'class2'))

    val <- import_classes_from('pkg', c('class1', 'class 2'))
    expect_identical( format(val)
                    , c( 'importClassesFrom(class1)'
                       , 'importClassesFrom("class 2")'
                       ))
})
#line 343 "/rdtf/documentation/R/Class-NamespaceEntry.R"
test_that('Class: ImportMethodsFrom', {#@testing Class: ImportMethodsFrom
    expect_error(import_methods_from())
    expect_error(import_methods_from('pkg'))
    expect_error(import_methods_from('pkg', ''))

    val <- import_methods_from('pkg', 'my_method')
    expect_is(val, 'ImportMethodsFrom')
    expect_equal(val@package, 'pkg')
    expect_equal(val@names, 'my_method')

    val <- import_methods_from('pkg', c('method1', 'method2'))
    expect_is(val, 'ImportMethodsFrom')
    expect_equal(val@package, 'pkg')
    expect_equal(val@names, c('method1', 'method2'))

    expect_identical( format(val)
                    , c( "importMethodsFrom(pkg,method1)"
                       , "importMethodsFrom(pkg,method2)"))
})
#line 367 "/rdtf/documentation/R/Class-NamespaceEntry.R"
test_that('namespace_quote', {#@testing
    expect_identical(namespace_quote(character(0)), character(0))
    expect_identical( namespace_quote(c('hi', 'hello world'))
                    , c('hi', '\"hello world\"')
                    )
})
