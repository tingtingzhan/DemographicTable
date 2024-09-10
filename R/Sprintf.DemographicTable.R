
#' @title Short Paragraph to Describe a [DemographicTable]
#' 
#' @description
#' To create a short paragraph to describe a [DemographicTable]
#' 
#' @param model a [DemographicTable]
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' Function [Sprintf.DemographicTable] returns a \link[base]{noquote} \link[base]{character} scalar.
#' 
#' @examples 
#' Sprintf.DemographicTable(DemographicTable(esoph))
#' Sprintf.DemographicTable(DemographicTable(ToothGrowth, groups = 'supp', include = 'len'))
#' 
#' @export Sprintf.DemographicTable
#' @export
Sprintf.DemographicTable <- function(model, ...) {
  # x0 <- unclass(model)
  
  data.name <- attr(model, which = 'data.name', exact = TRUE)
    
  ret <- if (length(groups <- attr(model, which = 'groups', exact = TRUE))) {
    sprintf(fmt = 'Descriptive statistics, e.g., means, medians, standard deviations, inter-quartile ranges (IQR) and percentages, of all subjects in dataset `%s`, as well as for each group of %s, are provided using <u>**`R`**</u>.',
            data.name,
            paste0('`', groups, '`', collapse = ', '))
      
  } else sprintf(fmt = 'Descriptive statistics, e.g., means, medians, standard deviations, inter-quartile ranges (IQR) and percentages, of all subjects in dataset `%s` are provided using <u>**`R`**</u>.', data.name)
  
  return(noquote(ret))
  
}

