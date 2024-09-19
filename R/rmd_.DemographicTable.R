
#' @title Create R Markdown Script for [DemographicTable]
#' 
#' @description
#' Method dispatch to [DemographicTable] for S3 generic `rmd_` (in a secret master package).
#' 
#' @param x a [DemographicTable]
#' 
#' @param xnm \link[base]{character} scalar, call of `x`
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
rmd_.DemographicTable <- function(x, xnm, type = 'html', ...) {
  
  data.name <- attr(x, which = 'data.name', exact = TRUE)
  
  return(c(
    
    if (length(groups <- attr(x, which = 'groups', exact = TRUE))) {
      sprintf(fmt = 'Descriptive statistics, e.g., means, medians, standard deviations, inter-quartile ranges (IQR) and percentages, of all subjects in dataset `%s`, as well as for each group of %s, are provided using <u>**`R`**</u>.',
              data.name,
              paste0('`', groups, '`', collapse = ', '))
      
    } else sprintf(fmt = 'Descriptive statistics, e.g., means, medians, standard deviations, inter-quartile ranges (IQR) and percentages, of all subjects in dataset `%s` are provided using <u>**`R`**</u>.', data.name),
    
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