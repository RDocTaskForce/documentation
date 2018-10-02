#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `zzz-Finalize.R`')
#line 7 "R/zzz-Finalize.R"
test_that('documentation.results', {#@testing
    documented <- dplyr::filter(documentation.results, Documented & (Reason==''))
    env <- asNamespace('documentation')

    for (name in documented$Name)
        expect_true(!is.null(documentation(get(name, envir = env))))

    dplyr::filter(documentation.results, !Documented) %>%
        dplyr::count(Message)
})
