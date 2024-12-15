
#' @title Create R Markdown Script for [DemographicTable]
#' 
#' @description
#' Method dispatch to [DemographicTable] for S3 generic `rmd_` (in a secret master package).
#' 
#' @param x a [DemographicTable]
#' 
#' @param xnm \link[base]{character} scalar, call of `x`
#' 
#' @param autofold \link[base]{logical} scalar
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' Function [rmd_.DemographicTable] returns a \link[base]{character} \link[base]{vector}.
#' 
#' @keywords internal
#' @export rmd_.DemographicTable
#' @export
rmd_.DemographicTable <- function(x, xnm, autofold = TRUE, ...) {
  
  dnm. <- vapply(x, FUN = attr, which = 'data.name', exact = TRUE, FUN.VALUE = '')
  dnm <- paste0('`', unique.default(dnm.), '`', collapse = ', ')
  
  grp. <- vapply(x, FUN = attr, which = 'group', exact = TRUE, FUN.VALUE = '')
  grp <- unique.default(grp.[nzchar(grp.)])
  
  return(c(
    
    paste0(
      'Descriptive statistics, e.g., means, medians, standard deviations, inter-quartile ranges (IQR) and percentages, ',
      if (length(grp)) paste0('per group of ', paste0('`', grp, '`', collapse = ', '), ' '), # else NULL
      sprintf(fmt = 'in dataset(s) %s ', dnm),
      'are provided using <u>**`R`**</u>.'
    ),
    
    if (autofold) '<details><summary>**Expand for Demographic Table**</summary>',
    '```{r results = \'asis\'}', 
    'flextable::set_flextable_defaults(font.size = 9)',
    sprintf(fmt = 'as_flextable.DemographicTable(%s)', xnm), 
    'flextable::init_flextable_defaults()',
    '```', 
    '</details>',
    
    '<any-text>'
    
  ))
}