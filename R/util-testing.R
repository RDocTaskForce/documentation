#' @include util-infix.R
expect_rd_output <- function(rd, file, info=NULL, label=NULL){
    act <- testthat::quasi_label(rlang::enquo(rd), label)

    val <- stringi::stri_split_lines1(collapse0(rd))
    expected <- readLines(system.file("expected_output", file, package='documentation'))
    ident <- identical(val, expected)
    msg <- if (ident) '' else ""
        sprintf("%s does not produce lines of documentation in %s", act$label, sQuote(file))
    testthat::expect(ident, msg,info=info)
}

