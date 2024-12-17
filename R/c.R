
#' @title Concatenate [DemographicTable] Objects
#' 
#' @param ... one or more [DemographicTable] objects
#' 
#' @returns
#' Function [c.DemographicTable] returns a [DemographicTable] object.
#' 
#' @keywords internal
#' @export c.DemographicTable
#' @export
c.DemographicTable <- function(...) {
  ret <- do.call(c, args = lapply(list(...), FUN = unclass))
  class(ret) <- c('DemographicTable', class(ret))
  return(ret)
}
