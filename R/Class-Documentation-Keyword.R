#' @include utils.R


keyword.db <- local({
keyword.lines <- readLines(normalizePath(file.path(R.home(), 'doc', 'KEYWORDS.db')))
keyword.descriptions <- gsub("^\\s+", "", sapply(strsplit(keyword.lines, ':', fixed=TRUE), getElement, 2))

keyword.heirarchy   <- strsplit(sapply(strsplit(keyword.lines, ':', fixed=TRUE), head, 1), '|', fixed=TRUE)
keyword             <- sapply( keyword.heirarchy, tail, 1)
keyword.parent.list <- sapply(sapply( keyword.heirarchy, tail, -1), tail, 1)
keyword.parent      <- unlist(ifelse( sapply( keyword.parent.list, length), keyword.parent.list, NA_character_))

db <-
data.frame( KEYWORD     = keyword
          , PARENT      = keyword.parent
          , DESCRIPTION = keyword.descriptions
          )
rownames(db) = keyword
db
})

#' @export
keyword <-
setClass("Documentation-Keyword", contains='character'
        , validity = function(object){
                is.valid <- object %in% keyword.db$KEYWORD
                if(all(is.valid)) return(TRUE)
                paste("the following are not valid keywords:", comma_list(object[!is.valid]))
            }
        )
if(FALSE){#! @testing
    x <- new('Documentation-Keyword', 'utilities')
    expect_true(validObject(x))
    expect_error(new('Documentation-Keyword', 'utils'))
    expect_true(validObject(as('utilities', 'Documentation-Keyword')))
}

#' @export
`c.Documentation-Keyword` <- function(...)as(NextMethod(), 'Documentation-Keyword')
#' @export
`sort.Documentation-Keyword` <- function(...)as(NextMethod(), 'Documentation-Keyword')
#' @export
`unique.Documentation-Keyword` <- function(...)as(NextMethod(), 'Documentation-Keyword')
#' @export
setAs( 'character', 'Documentation-Keyword'
     , function(from)new('Documentation-Keyword', from))
if(FALSE){#@testing
    val <- keyword('programming')
    val2 <- c(val, 'methods')
    expect_is(val2, 'Documentation-Keyword')
    expect_length(val2, 2L)

    expect_error( val3 <- c(val2, 'not a keyword')
                , "the following are not valid keywords")
}