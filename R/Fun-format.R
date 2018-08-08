#' @import assertthat
#' @include Classes.R
#' @include Fun-toRd.R


.documentation_formatters <- new.env(hash = TRUE)


#' Define a documentation formatter function
#'
#' Formatters are functions that given a Documentation object
#' and options return a character vector of lines to output to
#' the console or to a file.  Note that formats are not case
#' sensitive formats 'HTML' and 'HtMl' are equivalent.
#'
#' @param format the format(s) to map to a specific function.
#' @param fun the function or name of a function to use to format.
#' @param dir the default directory output should be directed to.
#' @param ext the default file extention to use.
#' @param overwrite allow overwritting?
#' @export
set_formatter<-
function(format, fun, dir = '.', ext = paste0('.', format[[1]]), overwrite=NA){
    assert_that(is.character(format), is.string(dir), is.string(ext))
    if(is.string(fun))
        fun <- match.fun(fun)
    assert_that(is.function(fun))
    info <- list(fun=fun, default.dir = dir, ext=ext)
    for(fmt in unique(tolower(format))){
        if (exists(fmt, envir = .documentation_formatters)){
            if (identical(get(fmt, envir=.documentation_formatters), info))
                doc_message(._("Formatter already recorded for '%s'.", fmt)
                           , type ="format-overwrite"
                           )
            else
                doc_condition(._("Formatter already exists for '%s'.", fmt)
                             , overwrite
                             , type ="format-overwrite"
                             )
        }
        assign(fmt, info, envir=.documentation_formatters)
    }
    invisible(NULL)
}

set_formatter(c("Rd", "toRd", ".Rd"), toRd, 'man', '.Rd')


get_formatter <-
function(format){
    tryCatch( match.fun(get( tolower(format)
                           , envir=.documentation_formatters
                           , inherits = FALSE)$fun)
    , error = function(e){
        tryCatch( if(is.function(fun <- match.fun(format, descend = FALSE))){
                        doc_warning(._("formatter for `%s` is not defined" %<<%
                                       "using function named `%s`.", format, format)
                                   , type = 'format-not_defined'
                                   )
                        return(fun)
                    }
                , error = function(e){
                    doc_error(._("formatter for '%s' is not defined.", format)
                             , type = "format-not_defined" )
                })
    })
}
if(FALSE){#@testing
    expect_identical(get_formatter('toRd'), toRd)
    expect_identical(get_formatter('Rd'), toRd)

    expect_error( get_formatter('not a format')
                , class = "documentation-error-format-not_defined"
                )
}

get_formatter_ext <-
function(format, ifmissing=paste0('.', format)){
    assert_that(is.string(format), is.string(ifmissing))
    if (exists(tolower(format), envir=.documentation_formatters, inherits = FALSE))
        return(get(tolower(format), envir=.documentation_formatters, inherits = FALSE)$ext)

    doc_warning(._("formatter for '%s' is not defined.", format)
                  , type = "format-not_defined" )
    return(ifmissing)
}
if(FALSE){#@testing
    expect_identical(get_formatter_ext('toRd'), '.Rd')
    expect_identical(get_formatter_ext('Rd'), '.Rd')

    expect_warning( value <- get_formatter_ext('not_a_format')
                  , class = "documentation-error-format-not_defined"
                  )
    expect_identical( value, '.not_a_format')
}

get_formatter_dir <- function(format, ifmissing='.'){
    assert_that(is.string(format), is.string(ifmissing))
    if (exists(tolower(format), envir=.documentation_formatters))
        return(get(tolower(format), envir=.documentation_formatters)$default.dir)

    doc_warning(._("formatter for '%s' is not defined.", format)
                  , type = "format-not_defined" )
    return(ifmissing)
}
if(FALSE){#@testing
    expect_identical(get_formatter_dir('toRd'), 'man')
    expect_identical(get_formatter_dir('Rd'), 'man')

    expect_warning( value <- get_formatter_dir('not_a_format')
                  , class = "documentation-error-format-not_defined"
                  )
    expect_identical(value, '.')
}



#' Format a documentation object.
#'
#' The format.Documentation function passed everything through to the formatter
#' function obtained through a call to `get_formatter(fmt)`.
#'
#' @seealso set_formatter
#' @export
format.Documentation <-
function( x  #< Documentation object
        , fmt = getOption("documentation::format", 'Rd')
            #< A format for documentation to be output as,
            #< see get_formatter.
        , ...
            #< options passed to formatter.
        ){
    formatter <- get_formatter(fmt)
    as.character(formatter(x, ...))
}
