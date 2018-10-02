#' @include setup.R
#' @include utils.R
#' @include Classes.R
#' @include Funs.R

documentation.results <- document_env(environment())
if(FALSE){#@testing
    documented <- dplyr::filter(documentation.results, Documented & (Reason==''))
    env <- asNamespace('documentation')

    for (name in documented$Name)
        expect_true(!is.null(documentation(get(name, envir = env))))

    dplyr::filter(documentation.results, !Documented) %>%
        dplyr::count(Message)
}
if(FALSE){# cleanup and development
    withr::with_options(list(warn=2, error=recover),
        extract_documentation(document_package)
    )

}
