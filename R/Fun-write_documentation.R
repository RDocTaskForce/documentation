#' @include Classes.R

#' Write all documentation for a package.
#'
#' This scans through all objects in a package environment
#' and writes documentation files to the given directory.
#' @export
write_package_documentation <-
function( pkg = '.'      #< Base path of package to document.
        , ...            #< arguments passed on to write_documentation.
        , extract = TRUE #< Should documentation be extracted as part of the process.
        , exclude = NULL
        , only.exports = TRUE
        ){
    #' @inheritParams document_package
    #' @inheritParams document_env
    # nocov start
    pkg <- devtools::as.package(pkg)

    pkg.env <- asNamespace(pkg$package)
    if (extract) document_package(pkg, exclude=exclude, only.exports=only.exports)
    if (!("package:" %<<<% pkg$package %in% search()))
        devtools::load_all(pkg, export_all = FALSE)
    env <- asNamespace(pkg$package)
    doc.objects <- collect_documentation_objects(env)
    for (doc in doc.objects)
        write_documentation(doc, ...)
    invisible(lapply(doc.objects, doc_get_aliases))
    # nocov end
}
if(FALSE){#@development
    pkg <- as.package('.')

    write_package_documentation(pkg, fmt='Rd', only.exports=FALSE)

    env <- asNamespace(pkg$package)
    doc.objects <- collect_documentation_objects(env)
    length(doc.objects)

    doc <- doc.objects[[1]]

    for (doc in doc.objects)
        write_documentation(doc, fmt='Rd', dir='man')
}


#' Write a documentation objec to file.
#' @export
write_documentation <-
function( doc    #< A Documentation object.
        , ...    #< passed on to format
        , fmt = getOption("documentation::format", 'Rd')  #< format to write documentation in.
        , file = file.path(dir, as.filename(doc_get_name(doc), ext))  #< file to write documenation to.
        , dir = get_formatter_dir(fmt)                    #< directory to write file to. Not used if file is provided.
        , ext = get_formatter_ext(fmt)                    #< extension to write file to. Not used if file is provided.
        , header.lines =  "% Generated by documentation: do not edit by hand"
            #< Lines to include in each file.
            #TODO extend to include reference to original filename.
        ){
    assert_that( is(doc, 'Documentation')
               , is.string(ext)
               , is.string(dir)
               , is.string(file) || is(file, "connection")
               , is.character(header.lines)
               )
    lines <- format(doc, fmt=fmt, ...)
    cat(lines, file=file, sep='')
}
if(FALSE){
    doc <- function_documentation( name = 'test'
                                 , title = "test title"
                                 , description = "A description for my test function"
                                 , arguments = ArgumentList( arg_('x', 'the x argument'))
                                 )
    toRd(doc)
    format(doc, 'Rd') %>% collapse0() %>% cat

    tmp <- tempfile('docs', fileext = '.Rd')

    txt <- textConnection('my_txt', 'w')
    write_documentation(doc, fmt='Rd', file = txt)
    close(txt)

    expect_true( "\\name{test}" %in% my_txt )
    expect_true( "\\arguments{\\item{x}{the x argument}}" %in% my_txt )
    expect_true( "\\usage{test(x)}" %in% my_txt )
    expect_true( "\\title{test title}" %in% my_txt )
    expect_true( "\\description{A description for my test function}" %in% my_txt )
    expect_false( "\\value{NA}" %in% my_txt
                , label = "Output included blank value")
    rm(list='my_txt')


    doc <- function_documentation( name = (name <- stringi::stri_rand_strings(1, 10, "[a-z]"))
              , title = (title <- stringi::stri_wrap(stringi::stri_rand_lipsum(1), 40)[[1]])
              , description = (description <- FormattedText(stringi::stri_rand_lipsum(3)))
              , arguments = ArgumentList()
              )

    write_documentation(doc, fmt='Rd', file = textConnection('test_documentation', 'w'))
    expect_identical( test_documentation
                    , c( "\\name{" %<<<% name %<<<% "}"
                       , "\\title{" %<<<% title %<<<% "}"
                       , "\\description{"
                       , head(interleave(description, c('', '', '')), -1)
                       , "}"
                       , "\\usage{" %<<<% name %<<<% "()}"
                       )
                    )
    rm(list='test_documentation')


    doc <- new('option-Documentation', 'anOption', 'a description')

    expect_identical(doc_get_name(doc), "anOption-option")
    expect_identical(doc_get_title(doc), "Documentation for Option 'anOption'")

    x <- textConnection("anOption-option.Rd", 'w')
    write_documentation(doc, fmt='Rd', file = x)
    close(x)

    expect_identical( `anOption-option.Rd`
                    , c( "\\name{anOption-option}"
                       , "\\alias{anOption}"
                       , "\\alias{anOption-option}"
                       , "\\title{Documentation for Option 'anOption'}"
                       , "\\description{a description}"
                       ))




}



#' Get all documentation objects from an environment
#'
#' This function collects all the documentation objects
#' from an environment, typically a package namespace,
#' whether from documented objects or from standalone
#' documentation objects.
#'
#' @export
collect_documentation_objects <-
function(env){
    is.namespace <- isNamespace(env)
    all.objects <- objects(envir=env, all.names = TRUE)
    all.docs <- lapply(all.objects, function(obj) {
        object <- get(obj, envir=env, inherits = FALSE)
        if (inherits(object, "Documentation"))
            return(object)
        else if (!is.null(doc <- rlang::eval_tidy(rlang::quo(documentation(!!(rlang::sym(obj)))), env)))
            return(doc)
        else if (!is.null(doc <- attr(object, "documentation")))
            return(doc)
        else
            return(NULL)
    })
    names(all.docs) <- all.objects
    all.docs <- Filter(Negate(is.null), all.docs)
    return (all.docs)
}
if(FALSE){# Development
    env <- asNamespace('documentation')
    doc.objects <- collect_documentation_objects(env)
    length(doc.objects)
    names(doc.objects)

    obj <- 'collect_documentation_objects'

    expr <- rlang::quo(documentation(!!(rlang::sym(obj))))
    rlang::eval_tidy(expr, env)

    documentation(collect_documentation_objects)
    extract_documentation(collect_documentation_objects)

}


#' Convert a string to a valid filename.
#'
#' Converts a string to a valid portable filename.
#' According to the Writing R Extentions manual,
#' valid filenames:
#'
#' * "For maximum portability filenames should contain only ASCII characters..."
#'   - `A-Za-z0-9._!#&+@^`
#' * cannot contain ACSII control characters.
#' * cannot contain the characters: ", \*, :, /, \<, \>, ?, \\, |,
#' * Should not include spaces.
#' * Should not include % as file names may be used in URLs.
#' * "It would be good practice to avoid the shell metacharacters (){}'[]$~:"
#'   and here we exclude them.
#' * The following are excluded for windows filenames and thus disallowed;
#'   - slash (/) and backslash (\\)
#'   - question mark (?)
#'   - vertical bar (|)
#'   - angle brackets (< and >)
#'   - collon (:)
#'   - asterisk (*)
#'   - quotation mark (")
#'   - plus sign (+)
#'   - comma (,)
#'   - semicolon (;)
#'   - equals sign (=)
#'   - square brackets (\[ and \])
#'
#'
#' @seealso
#'   * [Naming Files, Paths, and Namespaces on MSDN](https://docs.microsoft.com/en-us/windows/desktop/FileIO/naming-a-file)
#'   * [Filename on MSDN](https://docs.microsoft.com/en-us/windows/desktop/msi/filename)
#' @export
as.filename <-
function( str, ext='', space = '_', invalid=''){
    #' @param str the string to convert.
    #' @param ext file extension to add.
    #' @param space  the character to replace spaces with.
    #' @param invalid  the character to replace invalid elements with.
    str <- stringi::stri_enc_toascii(as.character(str))
    str <- gsub(' ', space, str)
    str <- gsub("[^A-Za-z0-9._!#&+@^]", invalid, str)
    if (grepl("[^A-Za-z0-9._!#&+@^]", ext))
        doc_error(._("ext contains invalid characters"))
    return(paste0(str, ext))
}
if(FALSE){
    expect_identical(as.filename("hello world", ".Rd"), "hello_world.Rd")
    expect_identical(as.filename("hello ///", ".Rd"), "hello_.Rd")
    expect_identical(as.filename("hello \x01", ".html"), "hello_.html")
    expect_error(as.filename("hello world", ".*"))
}
