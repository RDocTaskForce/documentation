#' @include Classes.R
#' @importFrom utils head tail


trimws_example <- function(txt){
    txt <- gsub("^\\s+$", "", txt)

    while (length(txt)>1 && head(txt, 1)=='') txt <- txt[-1]
    while (length(txt)>1 && tail(txt, 1)=='') txt <- head(txt, -1)


    prefixes <- regmatches(regexpr("^\\s*", txt[nchar(txt)>0]), x=txt[nchar(txt)>0])
    n <- min(nchar(prefixes))
    if (n > 0L) stringi::stri_sub(txt, to=n) <- ''

    txt[nchar(txt) | c(nchar(tail(txt, -1L)), TRUE)]
}
if(FALSE){#@testing
    txt <- c( ""
            , "    # prints hello world."
            , "    hw()"
            , "    "
            , ""
            , "    {# bracketed code"
            , "        do_something()"
            , "    }"
            , "    "
            )
    expect_equal( trimws_example(txt)
                , c( "# prints hello world."
                   , "hw()"
                   , ""
                   , "{# bracketed code"
                   , "    do_something()"
                   , "}"
                   )
                )
}

doc_example_get_src_lines <-
function(src){
    if(is.list(src)){
        l <- lapply(src, doc_example_get_src_lines)
        return(flatten_lines(l))
    }
    assert_that(is(src, 'srcref'))
    start <- utils::getSrcLocation(src, 'line', TRUE)
    end   <- utils::getSrcLocation(src, 'line', FALSE)
    col1  <- utils::getSrcLocation(src, 'col', TRUE)
    col2  <- utils::getSrcLocation(src, 'col', FALSE)

    file <- attr(src, 'srcfile')
    assert_that(is(file, 'srcfile'))
    n <- nchar(file$lines)
    if (col1==1 || is_whitespace(substring(getSrcLines(file, start, start), 1L, col1-1L)))
        while (start>1L){
            line <- getSrcLines(file, start-1, start-1)
            if (!grepl("^\\s*#", line)) break
            start <- start - 1L
            col1 <- 1L
        }
    if (end <= length(n) && col2 == n[[end]])
        while (end < length(n)) {
            line <- getSrcLines(file, end+1, end+1)
            if (!grepl("^\\s*#", line)) break
            end <- end + 1L
            col2 <- n[[end]] %if% (end+1L <= length(n)) %otherwise% 0L
        }
    as.character( srcref(file, c(start, col1, end, col2, col1, col2))
                , useSource=TRUE)
}
if(FALSE){#@testing
    txt <-{""                                                             %\%
           "#' An example character vector"                               %\%
           "#'"                                                           %\%
           "#' This example is used to test extracting documentation from"%\%
           "#' a character vector which typically does not contain"       %\%
           "#' a source reference."                                       %\%
           "example_character <-"                                         %\%
           "    c( \"first\"  #< the first element"                       %\%
           "     , \"second\" #< the second element"                      %\%
           "     )"                                                       %\%
           "" %\%
           "# Comment before" %\%
           "  do_something()" %\%
           "# comment after"  %\%
           ""}
    p <- parse(text=txt, keep.source = TRUE)


    expect_equal( doc_example_get_src_lines(getSrcref(p)[[1]])
                , c( "#' An example character vector"
                   , "#'"
                   , "#' This example is used to test extracting documentation from"
                   , "#' a character vector which typically does not contain"
                   , "#' a source reference."
                   , "example_character <-"
                   , "    c( \"first\"  #< the first element"
                   , "     , \"second\" #< the second element"
                   , "     )"
                   ))

    src <- getSrcref(p)[[2]]
    expect_equal( doc_example_get_src_lines(getSrcref(p)[[2]])
                , c( "# Comment before"
                   , "  do_something()"
                   , "# comment after"
                   ))
    expect_equal( doc_example_get_src_lines( getSrcref(p) )
                , c( "#' An example character vector"
                   , "#'"
                   , "#' This example is used to test extracting documentation from"
                   , "#' a character vector which typically does not contain"
                   , "#' a source reference."
                   , "example_character <-"
                   , "    c( \"first\"  #< the first element"
                   , "     , \"second\" #< the second element"
                   , "     )"
                   , ""
                   , "# Comment before"
                   , "  do_something()"
                   , "# comment after"
                   ))

}

# toRd,example #################################################################
#' @export
setMethod('toRd', "example",
function( obj, ...
        , use.source = default(use.source, TRUE) #< use source lines over reconstructed.
        , trimws = TRUE  #< Trim whitespace from sourcelines.
        ){
    if (length(obj)==0) return(Rd(character(0)))
    lines <-
        if (use.source && !is.null(src <- attr(obj, 'wholeSrcref'))) {
            doc_example_get_src_lines(src)
        } else
        if (use.source && !is.null(src <- attr(obj, 'srcref'))) {
            doc_example_get_src_lines(src)
        } else {
            as.character(S3Part(obj, TRUE))
        }
    if (trimws)
        lines <- trimws_example(lines)
    lines <- undim(paste0(lines, ifelse(grepl("\n$", lines), '', '\n')))
    return(Rd_canonize_code(Rd_rcode(lines)))
})
if(FALSE){#@testing
    simple.text <- "
    # prints hello world.
    hw()
    "
    p <- parse(text=simple.text, keep.source=TRUE)
    ex <- new('example', p)

    expect_equal( toRd(ex, use.source=TRUE)
                , Rd( Rd_rcode( "# prints hello world.\n")
                    , Rd_rcode( "hw()\n")
                    ))

    ex2 <- new('example', expression(test(x,y)))
    expect_is(ex2, 'Documentation-example')
    expect_null(getSrcref(ex2))
    expect_identical(toRd(ex2), Rd(Rd_rcode(c("test(x, y)\n"))))

    ex3 <- new('example', expression( a <- "test"
                                    , b <- Rd(a)
                                    , expect_is(b, 'Rd')
                                    ))
    expect_equal( toRd(ex3)
                , Rd(Rd_rcode( 'a <- "test"\n')
                    ,Rd_rcode( 'b <- Rd(a)\n')
                    ,Rd_rcode( 'expect_is(b, "Rd")\n')
                    ))
}
if(FALSE){#@testing
    ex.blank <- new('example')
    expect_identical(toRd(ex.blank), Rd(character(0)))


    ex.file <- system.file("examples", "example_character.R", package='documentation')
    obj <- new('example', parse(ex.file, keep.source=TRUE))

    lines <- readLines(ex.file)
    expect_identical( toRd(obj)
                    , Rd_canonize(Rd_rcode(paste0(lines[nchar(lines)>0L], '\n')))
                    )

    whole.src <- attr(obj, 'wholeSrcref')
    attr(obj, 'wholeSrcref') <- NULL
    expect_true( is.null(src <- attr(obj, 'wholeSrcref')))
    expect_true(!is.null(src <- attr(obj, 'srcref')))
    expect_identical( toRd(obj)
                    , Rd_canonize(Rd_rcode(paste0(lines[nchar(lines)>0L], '\n')))
                    )
}

# toRd,Documentation-Examples ##################################################
#' @export
setMethod('toRd', "Documentation-Examples",
function( obj, ...) {
    if (length(obj)==0) return(Rd())
    content <- lapply(obj, toRd, ...)
    if (length(content) > 1L)
        content <- Rd_lines(content)
    content <- Rd_canonize(cl(content, 'Rd'), ...)
    Rd_examples(content=content)
})
if(FALSE){#@testing
    simple.text <- "
    # prints hello world.
    hw()
    "
    p <- parse(text=simple.text, keep.source=TRUE)
    ex <- new('example', p)
    ex2 <- new('example', expression(test(x,y)))
    ex3 <- new('example', expression( a <- "test"
                                    , b <- Rd(a)
                                    , expect_is(b, 'Rd')
                                    ))
    examples <- new('Documentation-Examples', list(ex, ex2, ex3))

    rd <- toRd(examples)
    expect_is_exactly(rd, 'Rd')
    expect_true(is_Rd_tag(rd[[1]], "\\examples"))
    expect_identical( collapse0(as.character(rd))
                    , "\\examples{" %\%
                      "# prints hello world." %\%
                      "hw()" %\%
                      '' %\%
                      "test(x, y)" %\%
                      '' %\%
                      'a <- "test"' %\%
                      'b <- Rd(a)' %\%
                      'expect_is(b, "Rd")' %\%
                      "}")

    rd <- toRd(examples, control=list(indent=TRUE, indent.with=space(4)))
    expect_is_exactly(rd, 'Rd')
    expect_true(is_Rd_tag(rd[[1]], "\\examples"))
    expect_identical( collapse0(rd)
                    , exp <- "\\examples{" %\%
                      "    # prints hello world." %\%
                      "    hw()" %\%
                      '' %\%
                      "    test(x, y)" %\%
                      '' %\%
                      '    a <- "test"' %\%
                      '    b <- Rd(a)' %\%
                      '    expect_is(b, "Rd")' %\%
                      "}"
                    )

    ex.blank <- new('Documentation-Examples')
    expect_identical(toRd(ex.blank), Rd())
}

