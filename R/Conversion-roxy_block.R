#' @include Classes.R

.roxy.namespace <- roxygen2::roclet_tags( roxygen2::roclet('namespace'))
.roxy.namespace.tags <- names(.roxy.namespace)

setOldClass('roxy_block')
setAs('roxy_block', 'Documentation', function(from){
    call <- attr(from, 'call')
    while (call[[1]] == as.name('<-'))
        call <- call[[3]]
    if (call[[1]] == as.name('function'))
        as(from, 'function-Documentation')
})

setAs('roxy_block', 'function-Documentation', function(from){
    docs <- new('function-Documentation')
    for (i in seq_along(from)) {
        switch( names(from)[[i]]
              , param = {
                    param <- from[[i]]
                    if (param$name %in% docs@arguments)
                        stop('Argument ', param$name, 'is already defined')
                    new.arg <- arg_(param$name, param$description)
                    docs@arguments[[param$name]] <- new.arg
              }
              ,'return'= {
                    docs@value <- as(from[[i]], 'FormattedText')
              }
              , 'keywords' = {
                    docs@keywords <- as(from[[i]], 'Documentation-Keyword')
              }
              , {
                    name <- names(from)[[i]]
                    if (name %in% .roxy.namespace.tags) next
                    if (name == "include") next
                    value <- from[[i]]
                    if (name %in% slotNames(functionDocumentation)){
                        if (functionDocumentation@slots[[name]] == 'FormattedText')
                            value <- as(value, 'FormattedText')
                        slot(docs, name) <- value
                    } else {
                        warning(name, ' is not a valid function-Documentation slot name.')
                    }
              }
            )
    }
    alias <- attr(from, 'object')$alias
    if (!is.null(alias))
        docs@name <- as.name(alias)
    return(docs)
})
if(FALSE){#@testing setAs,roxy_block,function-Documentation
    test.file <- system.file("examples", "example_function1.R", package='documentation')

    rd_blocks <- roxygen2::parse_file(test.file)
    from <- rd_blocks[[1]]

    expect_is(from, 'roxy_block')

    docs <- as(from, 'Documentation')
    expect_is(docs, 'Documentation')
    expect_is(docs, 'function-Documentation')
    expect_equal(docs@name, as.name("example_function1"))
    expect_equal(docs@title, "This is the title")
    expect_equal(docs@description, FormattedText("This is the description block.\nIt takes multiple lines."))
    expect_length(docs@arguments,1)
    expect_null(docs@arguments$x)
    expect_equal(docs@arguments$y, arg(y, "explicit documentation for y"))
}

