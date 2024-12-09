

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
#' @importFrom flextable as_flextable flextable autofit hline vline merge_v merge_h
#' @importFrom officer fp_border
#' @export as_flextable.DemographicTable
#' @export
as_flextable.DemographicTable <- function(x, ...) {
  
  rnm <- lapply(x, FUN = rownames)
  if (!all(duplicated(rnm)[-1L])) stop('rownames not all-same')
  
  x0 <- do.call(cbind, args = x)
  group <- vapply(x, FUN = attr, which = 'group', exact = TRUE, FUN.VALUE = '')
  nc <- vapply(x, FUN = ncol, FUN.VALUE = NA_integer_)
  dnm <- vapply(x, FUN = attr, which = 'data.name', exact = TRUE, FUN.VALUE = '')
  
  x1 <- data.frame(' ' = dimnames(x0)[[1L]], unclass(x0), row.names = NULL, check.names = FALSE, fix.empty.names = FALSE, stringsAsFactors = FALSE)
  xnm <- names(x1)
  if (sum(sig_id <- (xnm == 'Signif')) > 1L) {
    xnm[sig_id] <- paste0('Signif', seq_len(sum(sig_id)))
  }
  names(x1) <- xnm
  
  v_hard <- c(1L, 1L + cumsum(nc[-length(nc)]))
  v_soft <- setdiff(seq_len(sum(nc)), v_hard)
    
  ret0 <- x1 |> flextable() |> 
    autofit(part = 'all') |>
    hline(i = seq_len(dim(x0)[1L] - 1L)) |>
    vline(j = v_hard) |>
    vline(j = v_soft, border = fp_border(width = .5))

  ret1 <- if (any(nz_grp <- nzchar(group))) {
    group[!nz_grp] <- vapply(x[!nz_grp], FUN = colnames, FUN.VALUE = '')
    ret0 |> 
      add_header_row(values = c(' ', group), colwidths = c(1, nc), top = TRUE) |>
      merge_v(part = 'header') 
  } else ret0

  ret1 |> 
    add_header_row(values = c(' ', dnm), colwidths = c(1, nc), top = TRUE) |>
    merge_h(part = 'header') |>
    merge_v(part = 'header') |>
    align(i = NULL, j = NULL, align = 'center', part = 'header') 
  
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
