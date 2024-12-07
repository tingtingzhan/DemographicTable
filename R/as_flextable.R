

#' @title Convert [DemographicTable] to \link[flextable]{flextable}
#' 
#' @description 
#' Convert a [DemographicTable] to \link[flextable]{flextable} object.
#' 
#' @param x a [DemographicTable] object
#' 
#' @param ... potential additional parameters, not currently in use 
#' 
#' @returns 
#' 
#' Function [as_flextable.DemographicTable] returns a \link[flextable]{flextable} object.
#' 
#' @note
#' 
#' End user may use \link[flextable]{set_caption} to add a caption to the output demographic table.
#'
#' @importFrom flextable as_flextable flextable autofit hline vline merge_v
#' @export as_flextable.DemographicTable
#' @export
as_flextable.DemographicTable <- function(x, ...) {
  
  rnm <- lapply(x, FUN = rownames)
  if (!all(duplicated(rnm)[-1L])) stop('rownames not all-same')
  
  x0 <- do.call(cbind, args = x)
  group <- vapply(x, FUN = attr, which = 'group', exact = TRUE, FUN.VALUE = '')
  nc <- vapply(x, FUN = ncol, FUN.VALUE = NA_integer_)
  dnm0 <- vapply(x, FUN = attr, which = 'data.name', exact = TRUE, FUN.VALUE = '')
  if (!all(duplicated(dnm0)[-1L])) stop('rownames not all-same')
  dnm <- dnm0[1L]
  
  x1 <- data.frame(' ' = dimnames(x0)[[1L]], unclass(x0), row.names = NULL, check.names = FALSE, fix.empty.names = FALSE, stringsAsFactors = FALSE)
  xnm <- names(x1)
  xnm[1L] <- dnm
  if (sum(sig_id <- (xnm == 'Signif')) > 1L) {
    xnm[sig_id] <- paste0('Signif', seq_len(sum(sig_id)))
  }
  names(x1) <- xnm
  
  ret0 <- flextable(data = x1) |> 
    autofit(part = 'all') |>
    hline(i = seq_len(dim(x0)[1L] - 1L)) |>
    vline(j = c(1L, 1L + cumsum(nc[-length(nc)])))
  
  if (all(!nzchar(group))) return(ret0)
    
  id_n <- !nzchar(group) # to be filled with sample size
  group[id_n] <- vapply(x[id_n], FUN = colnames, FUN.VALUE = '')
  ret0 |> 
    add_header_row(values = c(dnm, group), colwidths = c(1, nc), top = TRUE) |>
    align(i = 1L, j = NULL, align = 'center', part = 'header') |>
    merge_v(part = 'header')
  
}


#' @importFrom flextable autofit flextable hline vline align add_header_row merge_v
# @export as_flextable.sumtab
#' @export
as_flextable.sumtab <- function(x, ...) {
  
  x1 <- data.frame(' ' = dimnames(x)[[1L]], unclass(x), row.names = NULL, check.names = FALSE, fix.empty.names = FALSE, stringsAsFactors = FALSE)
  dnm <- attr(x, which = 'data.name', exact = TRUE)
  names(x1)[1L] <- dnm
  
  ret0 <- flextable(data = x1) |> 
    autofit(part = 'all') |>
    hline(i = seq_len(dim(x)[1L] - 1L)) |>
    vline(j = 1L)
  
  group <- attr(x, which = 'group', exact = TRUE)
  #if (!length(group)) return(ret0)
  if (!nzchar(group)) return(ret0)
   
  ret0 |> 
    add_header_row(values = c(dnm, group), colwidths = c(1, ncol(x)), top = TRUE) |>
    align(i = 1L, j = NULL, align = 'center', part = 'header') |>
    merge_v(part = 'header')
}



# ?base::print'
# @export print.DemographicTable
#' @export
print.DemographicTable <- function(x, ...) print(as_flextable.DemographicTable(x, ...))

# @export print.sumtab
#' @export
print.sumtab <- function(x, ...) print(as_flextable.sumtab(x, ...))
