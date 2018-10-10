#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `Fun-accessors.R`')
#line 68 "/rdtf/documentation/R/Fun-accessors.R"
test_that('.define_generic_doc_accessor', {#@testing
    if (isNamespaceLoaded("my-test-package"))
        unloadNamespace('my-test-package')

    env <- devtools:::makeNamespace('my-test-package')
    env$.__NAMESPACE__.$imports <- list("documentation", 'methods'
                                       , documentation = s( getNamespaceExports('documentation')
                                                          , names = getNamespaceExports('documentation')
                                                          )
                                       , methods = s( getNamespaceExports('methods')
                                                    , names=getNamespaceExports('methods')
                                                    )
                                       )
    setPackageName('my-test-package', env)
    .define_generic_doc_accessor('test_slot', where = env)

    expect_true(exists('doc_get_test_slot', where = env))
    getter <- get('doc_get_test_slot', env)
    expect_is(getter, "nonstandardGenericFunction")

    expect_true(exists('doc_test_slot<-', where = env))
    setter <- get('doc_test_slot<-', env)
    expect_is(setter, "nonstandardGenericFunction")

    expect_true(exists('doc_has_test_slot', where = env))
    has <- get('doc_has_test_slot', env)
    expect_is(has, "standardGeneric")


    val <- .define_generic_doc_accessor(c('test1', 'test2'), where=env)
    expect_identical(val, list(TRUE, TRUE))
    expect_true(exists('doc_get_test1', where = env))
    expect_true(exists('doc_get_test2', where = env))

    myDoc <- setClass( 'myDoc', contains = 'Documentation'
                     , list(test_slot = 'name', test1='character')
                     , where = env
                     )
    setMethod('doc_get_test2', 'myDoc', function(doc)"prepare to die."
             , where = env )
    setMethod('doc_test_slot<-', 'myDoc', function(doc, value){
        value <- as.name(value)
        doc@test_slot <- value
        return(invisible(doc))
    }, where=env)

    env$doc <- myDoc( test_slot = as.name('Inigo Montoya')
                    , test1 = "you kill my father"
                    )

    expect_is(env$doc, 'myDoc')
    expect_true(with(env, doc_has_test_slot(doc)))
    expect_true(with(env, doc_has_test1(doc)))
    expect_true(with(env, doc_has_test2(doc)))

    expect_identical(with(env, doc_get_test_slot(doc)), "Inigo Montoya")
    expect_identical(with(env, doc_get_test1(doc)), "you kill my father")
    expect_identical(with(env, doc_get_test2(doc)), "prepare to die.")


    with(env, doc_test_slot(doc) <- "Thanos")
    expect_identical(with(env, doc_get_test_slot(doc)), "Thanos")
    with(env, doc_test1(doc) <- "I kill everyone.")
    expect_identical(with(env, doc_get_test1(doc)), "I kill everyone.")
    expect_error(with(env, doc_test2(doc) <- "I'll kill you to."))


    .define_generic_doc_accessor(c('bad_slot'), where=env)
    expect_error( with(env, doc_get_bad_slot(doc))
                , class='documentation-error-invalid-slot' )

    unloadNamespace('my-test-package')
})
#line 164 "/rdtf/documentation/R/Fun-accessors.R"
test_that('setGeneric("doc_has_name", ...)', {#@testing
    doc <- function_documentation()
    expect_false(doc_has_name(doc))
    doc_name(doc) <- 'test'
    expect_true(doc_has_name(doc))
    expect_identical(doc_get_name(doc), 'test')
})
#line 172 "/rdtf/documentation/R/Fun-accessors.R"
test_that('generic accessors', {#@testing generic accessors
    if (.document.generated){
        expect_is(doc <- documentation(doc_get_name), 'function-Documentation')
        expect_equal(doc@title, "Documentation accessor for name")
        expect_equal( doc@description
                    , FormattedText("This is an automatically generated function for accessing the" %<<%
                                    "name of a documentation object." %<<%
                                    "Specific methods may override the default behavior, " %<<%
                                    "especially when the name" %<<%
                                    "is expected to conform to a standard" %<<%
                                    "or is generated form other known information."))

        expect_is(doc <- documentation(doc_get_title), 'function-Documentation')
        expect_equal(doc@title, "Documentation accessor for title")
        expect_equal( doc@description
                    , FormattedText("This is an automatically generated function for accessing the" %<<%
                                    "title of a documentation object." %<<%
                                    "Specific methods may override the default behavior, " %<<%
                                    "especially when the title" %<<%
                                    "is expected to conform to a standard" %<<%
                                    "or is generated form other known information."))
    }
})
#line 195 "/rdtf/documentation/R/Fun-accessors.R"
test_that('doc_has_*', {#@testing doc_has_*
    doc <- function_documentation("test", title="Test me!")

    expect_true(doc_has_name(doc))
    expect_true(doc_has_title(doc))
    expect_true(doc_has_export(doc))
    expect_false(doc_has_description(doc))
    expect_false(doc_has_details(doc))
})
#line 215 "/rdtf/documentation/R/Fun-accessors.R"
test_that('doc_details<-,Documentation-method', {#@testing
    doc <- function_documentation(name='test-doc')
    det <- FT(stringi::stri_rand_lipsum(3))

    expect_null(doc_get_details(doc))
    doc_details(doc) <- det
    expect_identical(doc_get_details(doc), det)
})
#line 237 "/rdtf/documentation/R/Fun-accessors.R"
test_that('doc_has & doc_get', {#@testing doc_has & doc_get
    doc <- function_documentation( name = "Normal"
                                 , title = "The Normal Distribution"
                                 , aliases = c('rnorm', 'dnorm', 'pnorm', 'qnorm')
                                 )
    expect_true(doc_has(doc, 'name'))
    expect_true(doc_has(doc, 'title'))
    expect_true(doc_has(doc, 'aliases'))

    expect_identical(doc_get(doc, 'name'), "Normal")
    expect_identical(doc_get(doc, 'title'), "The Normal Distribution")
    expect_identical(doc_get(doc, 'aliases'), c('Normal', 'dnorm', 'pnorm', 'qnorm', 'rnorm'))

    doc <- S3method_documentation('test', 'me')

    expect_true(doc_has(doc, 'generic'))
    expect_true(doc_has(doc, 'signature'))
})
