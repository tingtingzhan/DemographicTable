
#' @title Create R Markdown Script for [DemographicTable]
#' 
#' @description
#' Method dispatch to [DemographicTable] for S3 generic `rmd_` (in a different master package).
#' 
#' @param x a [DemographicTable]
#' 
#' @param xnm \link[base]{language} or \link[base]{character} scalar, call of `x`
#' 
#' @param type ..
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @export rmd_.DemographicTable
#' @export
rmd_.DemographicTable <- function(x, xnm, type, ...) {
  return(c(
    #add_bib(Sprintf.DemographicTable(x)),
    Sprintf.DemographicTable(x),
    if (type == 'html') '<details><summary>**Expand for Demographic Table**</summary>',
    '```{r results = \'asis\'}', 
    'set_flextable_defaults(font.size = 9)', # ?flextable::set_flextable_defaults
    paste0('as_flextable.DemographicTable(', xnm, ')'), 
    'init_flextable_defaults()', # ?flextable::init_flextable_defaults
    '```', 
    '</details>',
    '<any-text>'
  ))
}