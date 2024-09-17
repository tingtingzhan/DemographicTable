
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
#' @returns 
#' Function [rmd_.DemographicTable] returns a \link[base]{character} \link[base]{vector}.
#' 
#' @export rmd_.DemographicTable
#' @export
rmd_.DemographicTable <- function(x, xnm, type, ...) {
  return(c(
    Sprintf.DemographicTable(x),
    if (type == 'html') '<details><summary>**Expand for Demographic Table**</summary>',
    '```{r results = \'asis\'}', 
    'flextable::set_flextable_defaults(font.size = 9)',
    paste0('as_flextable.DemographicTable(', xnm, ')'), 
    'flextable::init_flextable_defaults()',
    '```', 
    '</details>',
    '<any-text>'
  ))
}