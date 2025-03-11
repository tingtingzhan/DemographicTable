

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
#' @importFrom flextable as_flextable flextable autofit bold color hline hline_bottom vline add_header_row add_footer_row merge_v merge_h
#' @importFrom officer fp_border
#' @importFrom scales pal_hue
#' @export as_flextable.DemographicTable
#' @export
as_flextable.DemographicTable <- function(x, ...) {
  
  rnm <- lapply(x, FUN = rownames)
  if (!all(duplicated(rnm)[-1L])) stop('rownames not all-same')
  
  x0 <- do.call(cbind, args = x)
  
  group. <- unlist(lapply(x, FUN = attr, which = 'group', exact = TRUE), use.names = TRUE) # *not* ?base::vapply (which does not retain names)
  group <- names(group.) # `names('')` returns NULL
  if (!length(group)) group <- ''
  if (any(z_grp <- !nzchar(group))) {
    group[z_grp] <- vapply(x[z_grp], FUN = colnames, FUN.VALUE = '')
  }
  
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
  v1_hue <- lapply(seq_along(nc), FUN = function(i) {
    j <- nc[i]
    if (j == 1L) return(integer())
    prev <- if (i == 1L) 0L else sum(nc[seq_len(i-1L)])
    prev + seq_len(j - if (compare[i]) 1L else 0L) + 1L # first column being variable names
  })
  v2_hue <- v1_hue[lengths(v1_hue) > 0L] # columns to have color
  hue_color <- unlist(lapply(lengths(v2_hue), FUN = pal_hue()), use.names = FALSE) # !length(v_hue) compatible
  v_hue <- if (length(v2_hue)) unlist(v2_hue) else numeric() # must; otherwise ?flextable::color error!!
   
  nr <- dim(x0)[1L]
  
  border_hard_ <- fp_border(width = 1.5, color = 'gray40')
  # *looks* like default border used in ?flextable::flextable
  # tzh does *not* know how to find out for sure, for now..
  # ?flextable:::print.flextable
  # ?flextable::htmltools_value
  border_soft_ <- fp_border(width = .5, color = 'gray40')
  
  x1 |> 
    flextable() |> 
    autofit(part = 'all') |>
    hline(i = seq_len(nr - 1L)) |> # `-1`: do not overwrite default bottom
    color(j = v_hue, color = hue_color, part = 'all') |>
    bold(j = v_hue, part = 'all') |>
    
    add_header_row(values = c(' ', group), colwidths = c(1, nc), top = TRUE) |>
    add_header_row(values = c(' ', dnm), colwidths = c(1, nc), top = TRUE) |>
    add_footer_row(values = c(' ', group), colwidths = c(1, nc), top = FALSE) |>
    hline_bottom(border = border_hard_, part = 'all') |> # to make sure each row of footer has a bottom
    add_footer_row(values = c(' ', dnm), colwidths = c(1, nc), top = FALSE) |>
    hline_bottom(border = border_hard_, part = 'all') |>
    
    # [vline]: must be after adding all 'footer' !
    vline(j = v_hard, border = border_hard_, part = 'all') |>
    vline(j = v_soft, border = border_soft_, part = 'all') |>
  
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
  
  ret0 <- x1 |>
    flextable() |> 
    autofit(part = 'all') |>
    hline(i = seq_len(dim(x)[1L] - 1L)) |>
    vline(j = 1L)
  
  group <- attr(x, which = 'group', exact = TRUE)
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
