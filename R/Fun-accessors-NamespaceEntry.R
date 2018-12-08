#' @include utils.R
#' @include Fun-accessors.R



setMethod('doc_get_name', 'NamespaceEntry', function(doc)format(doc))

# setMethod('doc_get_name', 'Export', function(doc)format.Export(doc))
# setMethod('doc_get_name', 'ExportPattern', function(doc)format.ExportPattern(doc))
# setMethod('doc_get_name', 'ExportS3method', function(doc)format.ExportS3method(doc))
# setMethod('doc_get_name', 'ExportS4methods', function(doc)format.ExportS4methods(doc))
# setMethod('doc_get_name', 'ExportS4class', function(doc)format.ExportS4class(doc))
#
# setMethod('doc_get_name', 'Import', function(doc)format.Import(doc))
# setMethod('doc_get_name', 'ImportFrom', function(doc)format.ImportFrom(doc))
# setMethod('doc_get_name', 'ImportClassesFrom', function(doc)format.ImportClassesFrom(doc))
# setMethod('doc_get_name', 'ImportMethodsFrom', function(doc)format.ImportMethodsFrom(doc))

