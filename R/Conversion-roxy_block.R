#' @include Classes.R

### Definitions #####
.roxy.namespace <- roxygen2::roclet_tags( roxygen2::roclet('namespace'))
.roxy.namespace.tags <- names(.roxy.namespace)

### roxy_block → Documentation #####
setAs('roxy_block', 'Documentation', function(from){
    call <- attr(from, 'call')
    while (call[[1]] == as.name('<-'))
        call <- call[[3]]
    if (call[[1]] == as.name('function'))
        as(from, 'function-Documentation')
})

### roxy_block → function-Documentation #####
setAs('roxy_block', 'function-Documentation', function(from){
    if ('rdname' %in% from)
        browser()


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
              , 'usage' = {
                    docs@usage <- as(parse(text=unlist(strsplit(from[[i]], '\n'))), 'usage')
              }
              , 'details' = {
                    docs@sections[['details']] <- as(from[[i]], 'FormattedText')
              }
              , 'describeIn' = {
                    attr(docs, 'describe.in') <- get(value$name)
                    docs@description <- as(value$description, 'FormattedText')
              }
              , {
                    name <- names(from)[[i]]
                    if (name %in% .roxy.namespace.tags) next
                    if (name == "include") next
                    value <- from[[i]]
                    if (name %in% slotNames(functionDocumentation)){
                        if (functionDocumentation@slots[[name]] == 'FormattedText')
                            value <- as(value, 'FormattedText')
                        slot(docs, name) <- as(value, getSlots(functionDocumentation)[name])
                    } else {
                        warning(name, ' is not a valid function-Documentation slot name.')
                    }
              }
            )
    }
    alias <- attr(from, 'object')$alias
    if (!is.null(alias)) {
        if (.is_undefined(docs@name))
            docs@name <- as.name(alias)
        else if (!identical(docs@name, alias)){
            doc_warning(._("documentation already has a name, %s," %<<%
                           "that doesn't match the alias, %s."
                          , dQuote(doc_get_name(docs)), dQuote(alias)))
            docs@aliases <- c(docs@aliases, alias)
        }
    }
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
if(FALSE){#@testing
    text <- "
    #' Testing name mismatch
    #'
    #' A description
    #'
    #' @name hello_world
    #' @aliases example_hello_world
    hw <- function( greeting = 'hello' #< What to say.
                  , who = 'world'      #< who to say it to.
                  ){
        cat(greeting, who)
    }
    "
    txt.roxy <- roxygen2::parse_text(text)[[1]]
    expect_warning( doc <- as(txt.roxy, 'function-Documentation')
                  , class="documentation-warning-roxy_block")

    expect_equal( doc_get_name(doc), "hello_world")
    expect_equal( doc@aliases, c("example_hello_world", "hw"))
    expect_equal( doc_get_aliases(doc), c("hello_world", "example_hello_world", "hw"))
}
