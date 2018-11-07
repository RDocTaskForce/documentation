#' @include util-infix.R

expect_is_not <-
function (object, class, info = NULL, label = NULL){
    stopifnot(is.character(class))
    act <- testthat::quasi_label(rlang::enquo(object), label)
    act$class <-
    exp_lab <- paste(class, collapse = "/")
    testthat::expect( Negate(inherits)(act$val, class)
                    , sprintf("%s is a %s; should not inherit from `%s`."
                             , act$lab, act$class, exp_lab)
                    , info = info)
    invisible(act$val)
}

expect_valid <-
function (object, complete=FALSE, info=NULL, label=NULL){
    act <- testthat::quasi_label(rlang::enquo(object), label)
    is.valid <- validObject(object, test=TRUE, complete=complete)
    testthat::expect(isTRUE(is.valid)
                    , ._("%s is not valid; %s", act$lab, dQuote(is.valid))
                    , info=info
                    )
}
if(FALSE){#@testing
    bad <- s(1L, class='Rd', Rd_tag='integer')
    expect_error( expect_valid(bad)
                , "`bad` is not valid;" %<<% dQuote("object is not a list")
                )

    good <- s(list(), class='Rd')
    expect_valid(good)
}


class0 <- function(x)collapse(class(x), '/')

all_inherit <- function(lst, what, label=NULL){
    act <- testthat::quasi_label(rlang::enquo(lst), label)
    stopifnot( is.character(what) || is.null(what) )
    if (all(. <- purrr::map_lgl(lst, inherits, what=what, which=FALSE)))
        return(TRUE)
    msg <- if (sum(!.) > 1L) {
        ._("%s has bad elements at %s which do not inherit from %s."
          , act$lab
          , comma_list(which(!.))
          , comma_list(dQuote(what), sep2 = ' or ', sep.last = ' or ')
          ) } else {
        bad.class <- purrr::map_chr(lst[!.], class0)
        ._("%s has bad element at %s which does not inherit from %s. It is a %s"
          , act$lab
          , comma_list(which(!.))
          , comma_list(dQuote(what), sep2 = ' or ', sep.last = ' or ')
          , dQuote(bad.class)
          )
          }
    return(s(FALSE, msg, bad.elements = which(!.)))
}
if(FALSE){#@ testing
    l <- list( 'a', 'b', 'c'
             , 1, 2
             , function()"hello world"
             )
    expect_identical( validate_that(all_inherit(l, 'character'))
                    , "`l` has bad elements at 4, 5, and 6" %<<%
                      "which do not inherit from" %<<%
                      dQuote("character") %<<<% '.')
    expect_identical( validate_that(all_inherit(l, c('character', 'function')))
                    , "`l` has bad elements at 4 and 5" %<<%
                      "which do not inherit from" %<<%
                      dQuote("character") %<<% 'or' %<<%
                      dQuote("function") %<<<% '.')
    expect_identical( validate_that(all_inherit(l, c('character', 'numeric')))
                    , "`l` has bad element at 6" %<<%
                      "which does not inherit from" %<<%
                      dQuote("character") %<<% 'or' %<<%
                      dQuote("numeric") %<<<% '.' %<<%
                      "It is a" %<<% dQuote("function"))

    expect_true( all_inherit(list(1L, 2L, 3L), 'integer'))
}
expect_all_inherit <- function (object, class, info = NULL, label = NULL) {
    act <- testthat::quasi_label(rlang::enquo(object), label)
    test <- all_inherit(object, class, label=act$lab)
    testthat::expect( isTRUE(test)
                    , attr(test, 'msg')
                    , info = info)
    invisible(test)
}
if(FALSE){
    expect_true( expect_all_inherit(1:3, 'integer'))
    l <- list( 'a', 'b', 'c'
             , 1, 2
             , function()"hello world"
             )
    expect_error( expect_all_inherit(l, 'character')
                , "`l` has bad elements at 4, 5, and 6" %<<%
                  "which do not inherit from" %<<%
                  dQuote("character") %<<<% '.')
    expect_error( expect_all_inherit(l, c('character', 'function'))
                , "`l` has bad elements at 4 and 5" %<<%
                  "which do not inherit from" %<<%
                  dQuote("character") %<<% 'or' %<<%
                  dQuote("function") %<<<% '.')
    expect_error( expect_all_inherit(l, c('character', 'numeric'))
                , "`l` has bad element at 6" %<<%
                  "which does not inherit from" %<<%
                  dQuote("character") %<<% 'or' %<<%
                  dQuote("numeric") %<<<% '.' %<<%
                  "It is a" %<<% dQuote("function"))


}

#' Vectorized version of is
are <- function(lst, class2){
    purrr:::map_lgl(lst, is, class2)
}
if(FALSE){#@testing
    lst <- list('a', 1L, TRUE)

    expect_true(all(are(lst, 'ANY')))
    expect_identical(are(lst, 'character'), c(T,F,F))
    expect_identical(are(lst, 'integer'), c(F,T,F))
    expect_identical(are(lst, 'numeric'), c(F,T,F))
}


all_are_exactly <- function(lst, what, label=NULL, exact=TRUE){
    act <- testthat::quasi_label(rlang::enquo(lst), label)
    stopifnot( is.string(what) )
    if (!exact) return(all_inherit(lst, what, label=act$label))
    if (all(. <- purrr::map_int(lst, inherits, what, which=TRUE) == 1L))
        return(TRUE)
    bad.class <- purrr::map_chr(lst[!.], class0)
    msg <- if (sum(!.) > 1L){
        ._("%s has bad elements at positions %s which are not of class %s."
          , act$lab
          , comma_list(which(!.))
          , dQuote(what)
          )} else {
        ._("%s has bad element at position %s which is not of class %s."
          , act$lab
          , which(!.)
          , dQuote(what)
        )}
    return(s(FALSE, msg))
}
if(FALSE){#@testing
    l <- list( 'a', 'b', 'c'
             , 1, 2)
    expect_identical( validate_that(all_are_exactly(l, 'character'))
                    , "`l` has bad elements at positions 4 and 5" %<<%
                      "which are not of class" %<<%
                      dQuote("character") %<<<% '.')
    expect_identical( validate_that(all_are_exactly(list(1,2), 'integer', '...'))
                    , "... has bad elements at positions 1 and 2" %<<%
                      "which are not of class" %<<%
                      dQuote("integer") %<<<% '.')
    expect_identical( validate_that(all_are_exactly(list(1L,2L), 'numeric', '...'))
                    , "... has bad elements at positions 1 and 2" %<<%
                      "which are not of class" %<<%
                      dQuote("numeric") %<<<% '.')
    expect_identical( validate_that(all_are_exactly(list(1, 2L), 'numeric', '...'))
                    , "... has bad element at position 2" %<<%
                      "which is not of class" %<<%
                      dQuote("numeric") %<<<% '.')
    expect_true(all_are_exactly(list(1L, 2L), 'integer'))
}


is_exactly <- function(x, what){any(inherits(x, what=what, which=TRUE)==1)}
if(FALSE){
    x <- Rd_text("text")
    expect_true(is_exactly(x, 'Rd_TEXT'))
    expect_true(is_exactly(x, c('Rd_RCODE', 'Rd_TEXT')))
    expect_false(is_exactly(Rd(x), c('Rd_RCODE', 'Rd_TEXT')))

    docs <- function_documentation()

    expect_true(is_exactly(docs, 'function-Documentation'))
    expect_false(is_exactly(docs, 'Documentation'))
}

expect_is_exactly <-
function (object, class, info = NULL, label = NULL){
    stopifnot(is.character(class))
    act <- testthat::quasi_label(rlang::enquo(object), label)
    act$class <- collapse(class(object), "/")
    exp_lab <- comma_list(class, sep2 = ' or ', sep.last = ', or a')
    testthat::expect( is_exactly(act$val, class)
                    , sprintf("%s is a %s; should be exactly a `%s`."
                             , act$lab, act$class, exp_lab)
                    , info = info)
    invisible(act$val)
}
if(FALSE){#@testing
    x <- list(1:3)

    expect_identical(expect_is_exactly(x, 'list'), x)

    class(x) <- c('class', 'super1', 'super2')

    expect_is_exactly(x, 'class')

    expect_is(x, 'super1')
    expect_error( expect_is_exactly(x, 'super1')
                , "`x` is a class/super1/super2; should be exactly a `super1`."
                )
}


expect_rd_output <- function(rd, file, info=NULL, label=NULL){
    act <- testthat::quasi_label(rlang::enquo(rd), label)

    val <- stringi::stri_split_lines1(collapse0(rd))
    expected <- readLines(system.file("expected_output", file, package='documentation'))
    ident <- identical(val, expected)
    msg <- if (ident) '' else ""
        sprintf("%s does not produce lines of documentation in %s", act$label, sQuote(file))
    testthat::expect(ident, msg,info=info)
}

is_valid_regex <- function(pattern){
    tryCatch( grepl(pattern, '') || TRUE
            , error= function(e)s(FALSE, msg=e$message))
}
if(FALSE){#@testing
    expect_true(is_valid_regex("^hello world$"))
    expect_false(is_valid_regex("^hello (world$"))
    expect_identical( validate_that(is_valid_regex("^hello (world$"))
                    , "invalid regular expression " %<<<%
                      "'^hello (world$', reason 'Missing ')''"
                    )
}

.nonempty.string.msg <- "does nto conform to a non-empty string" %<<%
                        "(a character vector of length 1 without" %<<%
                        "without missing or empty values)."

is_nonempty_string <- structure(function(x){
    is.character(x) &&
    length(x) == 1L &&
    !is.na(x)       &&
    nchar(x) > 0L
}, fail = function(call, env){
    sQuote(deparse(call$x)) %<<% .nonempty.string.msg
})
if(FALSE){#@testing
    expect_true(is_nonempty_string("hello world"))
    expect_false(is_nonempty_string(c("hello", "world")))
    expect_false(is_nonempty_string(character(0)))
    expect_false(is_nonempty_string(NA_character_))
    expect_false(is_nonempty_string(''))

    expect_identical( validate_that(is_nonempty_string(character(0)))
                    , sQuote("character(0)") %<<% .nonempty.string.msg)

    bad <- NA
    expect_identical( validate_that(is_nonempty_string(bad))
                    , sQuote("bad") %<<%.nonempty.string.msg)
}

.optional.string.msg <- "does not conform to an optional string" %<<%
                        "(a character vector of length 0 or 1," %<<%
                        "without missing or empty values)."

is_optional_string <- structure(function(x){
    is.character(x) &&
    length(x) <= 1L &&
    !any(is.na(x))  &&
    !any(nchar(x) == 0L)
}, fail = function(call, env){
    sQuote(deparse(call$x)) %<<% .optional.string.msg
})
if(FALSE){#@testing
    expect_true(is_optional_string("hello"))
    expect_true(is_optional_string(character(0)))
    expect_false(is_optional_string(NA_character_))
    expect_false(is_optional_string(''))
    expect_false(is_optional_string(letters))

    bad <- NA_character_
    expect_identical(validate_that(is_optional_string(bad))
                    , sQuote('bad') %<<% .optional.string.msg)
}


new_namespace_env <- function(name, path = file.path(tempdir(), name)){
    assert_that(!isNamespaceLoaded(name))
    new_sub_env <- function(part, parent=baseenv()){
        s( new.env(parent=parent, hash=TRUE)
         , name = part %<<<% ':' %<<<% name )
    }

    if (!dir.exists(path)) dir.create(path)
    path <- normalizePath(path, "/", TRUE)

    imports <- new_sub_env('imports')
    ns      <- new.env(TRUE, imports)
    ns$.__NAMESPACE__. <- new.env(parent = baseenv())
    ns$.__NAMESPACE__.$spec <- c(name = name, version = "0.0.0")
    setNamespaceInfo(ns, "exports"  , new_sub_env('exports'))
    setNamespaceInfo(ns, "lazydata" , new_sub_env('lazydata'))
    setNamespaceInfo(ns, "imports"  , list(base = TRUE))
    setNamespaceInfo(ns, "path"     , path)
    setNamespaceInfo(ns, "dynlibs"  , NULL)
    setNamespaceInfo(ns, "S3methods", matrix(NA_character_, 0L, 3L))
    ns$.__S3MethodsTable__. <- new.env(hash = TRUE, parent = baseenv())
    ns
}
if(FALSE){#@testing
    ns <- new_namespace_env("test package environment")
    expect_true(isNamespace(ns))
    expect_false(isNamespaceLoaded("test package environment"))
}
new_pkg_test_environment <- function(name = "test package environment"){
    env <- new_namespace_env(name)
    env$.__NAMESPACE__.$imports <-
        list("documentation", 'methods'
            , documentation = s( getNamespaceExports('documentation')
                               , names = getNamespaceExports('documentation')
                               )
            , methods = s( getNamespaceExports('methods')
                               , names=getNamespaceExports('methods')
                               )
            )
    setPackageName(name, env)
    return(env)
}
if(FALSE){#@testing
    ns <- new_pkg_test_environment()
    expect_true(isNamespace(ns))
    expect_equal(getPackageName(ns), "test package environment")
    expect_equal(environmentName(ns), "test package environment")
}



ngettextf <-
function( n
        , msg1
        , msg2
        , ...
        , domain=NULL
        ){
    if (n<=1) gettextf(msg1, ..., domain = domain)
         else gettextf(msg2, ..., domain = domain)
}

is_valid <- function(object, complete=FALSE){
    valid <- is_valid(object, test=TRUE, complete=complete)
    if(isTRUE(valid)) return(TRUE)
                 else return(s(FALSE, msg=valid))
}
are_valid <-
function(lst, complete=FALSE, simplify=NA){
    valid <- lapply(lst, is_valid, complete=complete)
    if (isFALSE(simplify)) return(valid) else
    if (isTRUE(simplify) || all(valid)) return(simplify2array(valid))
    else return(valid)
}

catch_condition <- function(code){
    val <- tryCatch(force(code), condition = function(cond)cond)
    if (is(val, 'condition')) return(val)
}
if(FALSE){#@testing
    val <- catch_condition(pkg_message("testing"))
    expect_is(val, 'condition')
    expect_is(val, 'message')
}
