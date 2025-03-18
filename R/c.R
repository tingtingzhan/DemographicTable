
#' @title Concatenate [DemographicTable] Objects
#' 
#' @param ... one or more [DemographicTable] objects
#' 
#' @returns
#' Function [c.DemographicTable()] returns a [DemographicTable] object.
#' 
#' @keywords internal
#' @export c.DemographicTable
#' @export
c.DemographicTable <- function(...) {
  ret <- list(...) |>
    lapply(FUN = unclass) |> # a 'list' of 'list' (not 'listof')
    do.call(what = c)
  class(ret) <- c('DemographicTable', 'listof')
  return(ret)
}
