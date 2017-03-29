#' @include Class-option-documentation.R

defaults <- new.env()
defaults$format_md <- list() 
defaults$format_md$indent <- FALSE

set_option_documentation( "documentation::format_md::indent"
   , description = "Determines if code should be indented when formatted.  Should default to FALSE when unset."
   , default     = as(expression(defaults$format_md$indent), 'Documentation-Default-Value')
   )


set_option_documentation( "documentation::format_md::indent.with"
   , description = "Determines what to indent with, when getOption('documentation::format_md::indent') is TRUE."
   , default     = as(expression(defaults$format_md$indent.with), 'Documentation-Default-Value')
   )


set_option_documentation("documentation::format_md::indent.with"
   , description = "Determines what to indent with, when getOption('documentation::format_md::indent') is TRUE."
   , default     = as(expression(defaults$format_md$indent.with), 'Documentation-Default-Value')
   )

set_option_documentation("documentation::verbose"
   , description = "Should verbose message be given?"
   , default     = FALSE
   )
