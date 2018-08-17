#' @include Classes.R

.not.found <- s(new.env(parent = emptyenv()), class='not found')

#' Inherited Options
#'
#' This gives the ability of setting options that
#' can inherit from a higher domain.
#' The heirachy comes from a format of naming,
#' so setting options stays the same but retrieving changes.
#'
#' Names are of the form `"domain_1::domain_2::...::domain_n::id`.
#' In this setup `domain_2` is nested in `domain_1` and so on.
#' If there are n domains `domain_n` is the most specific and `domain_1`
#' if the least specific.
#' `get_option` searches through the domains for the option with
#' name `id` for a value. If an option is not set it searches the
#' next higher domain by dropping the most specific domain from
#' the option specification. Finally, it searches for `id` without
#' any domain, assuming it to be the most general.
#'
#' For Example, if the option `"domain_1::verbose"` is set to `TRUE`
#' and the value of `"domain_1::domain_2::verbose"` is not set,
#' when the user requests the value of "domain_1::domain_2::verbose",
#' the value returned would be TRUE, the value of the option for
#' "domain_1::verbose", because the most specific is not set so the
#' next higher is searched and found.  Also note that in this case
#' if none of the options with domain specifiers are set the value for
#' `getOption('verbose')` would be returned, typically returning FALSE.
get_option <-
    function(name, default=NULL){
        assert_that(is.string(name))

        parts <- stringi::stri_split(name, fixed="::")[[1]]
        domains <- head(parts, -1)
        id <- tail(parts, 1)

        for( i in length(domains):0){
            spec <- collapse(c(head(domains, i), id), with='::')
            if (!identical(. <- getOption(spec, .not.found), .not.found))
                return(.)
        }
        default
    }
if(FALSE){#@ testing
    o <- list( "d1::d2::d3::a" = 1
               , "d1::d2::b"     = 2
               , "d1::c"         = 3
               , "e"             = 4
    )
    withr::with_options(o, {
        expect_identical(get_option("d1::d2::d3::a", .not.found), 1)
        expect_identical(get_option("d1::d2::a"    , .not.found), .not.found)
        expect_identical(get_option("d1::a"        , .not.found), .not.found)
        expect_identical(get_option("a"            , .not.found), .not.found)

        expect_identical(get_option("d1::d2::d3::b", .not.found), 2)
        expect_identical(get_option("d1::d2::b"    , .not.found), 2)
        expect_identical(get_option("d1::b"        , .not.found), .not.found)
        expect_identical(get_option("b"            , .not.found), .not.found)

        expect_identical(get_option("d1::d2::d3::c", .not.found), 3)
        expect_identical(get_option("d1::d2::c"    , .not.found), 3)
        expect_identical(get_option("d1::c"        , .not.found), 3)
        expect_identical(get_option("c"            , .not.found), .not.found)

        expect_identical(get_option("d1::d2::d3::e", .not.found), 4)
        expect_identical(get_option("d1::d2::e"    , .not.found), 4)
        expect_identical(get_option("d1::e"        , .not.found), 4)
        expect_identical(get_option("e"            , .not.found), 4)
    })
}
if(FALSE){#@example
    o <- list( "d1::d2::d3::a" = 1
               , "d1::d2::b"     = 2
               , "d1::c"         = 3
               , "e"             = 4
    )
    opar <- options(o)

    get_option("d1::d2::d3::a")
    get_option("d1::d2::d3::b")
    get_option("d1::d2::d3::c")
    get_option("d1::d2::d3::e")


    get_option("a", "not found")
    get_option("b", "not found")
    get_option("c", "not found")
    get_option("e")

    options(opar)
}



#' Get value from inherited option.
#'
#' `default` and `default_` expand on get_option for
#' arguments of a function.
#' The domains are:
#'
#'   #. The package name
#'   #. The calling function name.
#'   #. class of method if a S3 method.
#'   #. Other specifics as defined when called.
#'
#' @param name    Identifier
#' @param default The default value if not found
#' @param prefix  Domains to add before pkg::fun
#' @param suffix  Domains to add afer pkg::fun but before id
#' @param n       Number of calls offset. Used for determining pkg and fun if
#'                not provided.
#' @param fun     name of the relevant, for overwriting default.
#' @param pkg     name of the package, for overwriting default.
#'
#' @aliases default default_
#' @export
default_ <-
    function( name
            , default = NULL
            , n = 1L
            , prefix  = character(0)
            , pkg
            , fun
            , suffix  = character(0)
            ){
        call.no <- min(which(sapply(sys.frames(), identical, parent.frame(n))))
        S3 <- is_S3_method_call(call.no)
        S4 <- is_S4_method_call(call.no)
        if (missing(fun)) {
            fun <- as.character(sys.call(ifelse(S3, call.no-1L, call.no))[[1L]])
    } else if(!is.null(fun)){
        assert_that(is.string(fun))
    }
    if (missing(pkg)){
        pkg   = getPackageName(topenv(parent.frame(n)))
        if(pkg =='.GlobalEnv' || pkg == "base" || pkg == '') pkg <- NULL
    } else if(!is.null(pkg)){
        assert_that(is.string(pkg))
    }
    domains <- c( prefix
                , pkg
                , fun
                , if (S3) get_S3_method_specialization(call.no)
                , if (S4) get_S4_method_specialization(call.no)
                , suffix
                )
    return(get_option(collapse(c(domains, name), with="::"), default))
}
if(FALSE){#! @testing
    o <-list( 'documentation::default_test_function::test.arg' = 1
            , 'documentation::inherited.arg' = 2
            , 'global.arg' = "abc"
            )
    withr::with_options(o, {
        test_default <- function(name){
            default_(name, TRUE, fun='default_test_function', pkg='documentation')
        }
        expect_equal(test_default('test.arg')     , 1    )
        expect_equal(test_default('inherited.arg'), 2    )
        expect_equal(test_default('global.arg'   ), 'abc')
        expect_true (test_default('no.arg'       )      )
    })

    withr::with_options(list( 'default_test_function::test.arg' = 1
                            , 'inherited.arg' = 2
                            , 'global.arg' = 3
                            ), {
        default_test_function <-
        function( which = c('test', 'inherited', 'global', 'no')
                , test.arg      = default_('test.arg'      , FALSE, pkg=NULL)
                , inherited.arg = default_('inherited.arg' , FALSE, pkg=NULL)
                , global.arg    = default_('global.arg'    , FALSE, pkg=NULL)
                , no.arg        = default_('no.arg'        , FALSE, pkg=NULL)
                ){
            switch( match.arg(which)
                  , test      = test.arg
                  , inherited = inherited.arg
                  , global    = global.arg
                  , no        = no.arg
                  )
        }
        default_test_function('test')

        expect_equal(default_test_function('test'), 1)
        expect_equal(default_test_function('inherited'), 2)
        expect_equal(default_test_function('global'), 3)
        expect_equal(default_test_function('no'), FALSE)
    })

    print.my_class <- function( x
                              , ...
                              , test.arg      = default_('test.arg'      , FALSE
                                                        , prefix="DEFAULTS"
                                                        , pkg='test-documentation'
                                                        , suffix="OVERRIDE"
                                                        )
                              ){
        cat( "test.arg=" %<<<% test.arg %<<<% "\n")
        invisible(x)
    }

    if (interactive() && tracingState()) {
        trace(default_, exit =
                  substitute(doc_error("Documentation debugging\n"
                                      , domains=domains
                                      , type = "trace_test")))
        val <- tryCatch( print(s(list(), class='my_class'))
                       , "documentation-error-trace_test" = function(e){attr(e, 'domains')}
                       )
        expect_is(val, 'character')
        expect_length(val, 5)
        expect_identical( val, c("DEFAULTS", "test-documentation", "print", "my_class", "OVERRIDE"))
        untrace(default_)
    }
}

#' @export
default <-
function( arg
        , default = NULL
        , ...
        , n = 1
        ){
    name <- deparse(substitute(arg))
    default_( name=name, default=default
            , n=n+1L
            , ...
            )
}
if(FALSE){#! @testing
    opar <- options()
    options( 'documentation::default_test_function::test.arg' = 1
           , 'documentation::inherited.arg'                   = 2
           , 'global.arg' = "abc"
           )
    test_default1 <- function(x=default(test.arg      , TRUE, fun='default_test_function', pkg='documentation'))x
    test_default2 <- function(x=default(inherited.arg , TRUE, fun='default_test_function', pkg='documentation'))x
    test_default3 <- function(x=default(global.arg    , TRUE, fun='default_test_function', pkg='documentation'))x
    test_default4 <- function(x=default(no.arg        , TRUE, fun='default_test_function', pkg='documentation'))x

    expect_equal(test_default1(), 1)
    expect_equal(test_default2(), 2)
    expect_equal(test_default3(), 'abc')
    expect_true (test_default4())

    options(opar)
}

if(FALSE){
    undebug(default)
    debug(default)

    options(test_function)
}
