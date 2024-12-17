

#' @title Convert [DemographicTable] to \link[flextable]{flextable}
#' 
#' @description 
#' Convert a [DemographicTable] to a \link[flextable]{flextable} object.
#' 
#' @param x a [DemographicTable] object
#' 
#' @param ... additional parameters, not currently in use 
#' 
#' @returns 
#' Function [as_flextable.DemographicTable] returns a \link[flextable]{flextable}.
#' 
#' @note
#' End user may use function \link[flextable]{set_caption} to add a caption to the output demographic table.
#'
#' @keywords internal
#' @importFrom flextable as_flextable flextable autofit color hline vline add_header_row add_footer_row merge_v merge_h
#' @importFrom officer fp_border
#' @importFrom scales pal_hue
#' @export as_flextable.DemographicTable
#' @export
as_flextable.DemographicTable <- function(x, ...) {
  
  rnm <- lapply(x, FUN = rownames)
  if (!all(duplicated(rnm)[-1L])) stop('rownames not all-same')
  
  x0 <- do.call(cbind, args = x)
  group <- vapply(x, FUN = attr, which = 'group.name', exact = TRUE, FUN.VALUE = '')
  compare <- vapply(x, FUN = attr, which = 'compare', exact = TRUE, FUN.VALUE = NA)
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
  v_hue_ <- lapply(seq_along(nc), FUN = function(i) {
    j <- nc[i]
    if (j == 1L) return(integer())
    prev <- if (i == 1L) 0L else sum(nc[seq_len(i-1L)])
    prev + seq_len(j - if (compare[i]) 1L else 0L) + 1L # first column being variable names
  })
  v_hue <- v_hue_[lengths(v_hue_) > 0L]
    
  ret0 <- x1 |> 
    flextable() |> 
    autofit(part = 'all') |>
    hline(i = seq_len(dim(x0)[1L] - 1L)) |>
    vline(j = v_hard, border = fp_border(width = 1.5)) |>
    vline(j = v_soft, border = fp_border(width = .5))

  for (i in seq_along(v_hue)) { # len-0 compatible
    ret0 <- color(ret0, j = v_hue[[i]], color = pal_hue()(n = length(v_hue[[i]])), part = 'all')
  }

  ret1 <- if (any(nz_grp <- nzchar(group))) {
    group[!nz_grp] <- vapply(x[!nz_grp], FUN = colnames, FUN.VALUE = '')
    ret0 |> 
      add_header_row(values = c(' ', group), colwidths = c(1, nc), top = TRUE) |>
      add_footer_row(values = c(' ', group), colwidths = c(1, nc), top = FALSE)
  } else ret0

  ret1 |> 
    add_header_row(values = c(' ', dnm), colwidths = c(1, nc), top = TRUE) |>
    add_footer_row(values = c(' ', dnm), colwidths = c(1, nc), top = FALSE) |>
    merge_h(part = 'header') |>
    merge_v(part = 'header') |>
    merge_h(part = 'footer') |>
    merge_v(part = 'footer') |>
    color(i = 1:2, color = 'black', part = 'header') |> # having `v_hue` or not
    align(align = 'center', part = 'header') |>
    align(align = 'center', part = 'footer')
  
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
  
  group <- attr(x, which = 'group.name', exact = TRUE)
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
