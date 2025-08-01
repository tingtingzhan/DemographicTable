

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
#' Function [as_flextable.DemographicTable()] returns a \link[flextable]{flextable}.
#' 
#' @note
#' End user may use function \link[flextable]{set_caption} to add a caption to the output demographic table.
#'
#' @keywords internal
#' @importFrom flextable as_flextable flextable init_flextable_defaults autofit bold color hline hline_bottom vline add_header_row add_footer_row merge_v merge_h
#' @importFrom officer fp_border
#' @importFrom scales pal_hue
#' @export as_flextable.DemographicTable
#' @export
as_flextable.DemographicTable <- function(x, ...) {
  
  rnm <- lapply(x, FUN = rownames)
  if (!all(duplicated(rnm)[-1L])) stop('rownames not all-same')
  
  x0 <- do.call(cbind, args = x)
  
  group. <- x |>
    lapply(FUN = attr, which = 'group', exact = TRUE) |>
    unlist(use.names = TRUE) # *not* ?base::vapply (which does not retain names)
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
  v1_hue <- nc |>
    seq_along() |>
    lapply(FUN = \(i) {
      j <- nc[i]
      if (j == 1L) return(integer())
      prev <- if (i == 1L) 0L else sum(nc[seq_len(i-1L)])
      prev + seq_len(j - if (compare[i]) 1L else 0L) + 1L # first column being variable names
    })
  v2_hue <- v1_hue[lengths(v1_hue) > 0L] # columns to have color
  hue_color <- v2_hue |>
    lengths() |>
    lapply(FUN = pal_hue()) |>
    unlist(use.names = FALSE) # !length(v_hue) compatible
  v_hue <- if (length(v2_hue)) unlist(v2_hue) else numeric() # must; otherwise ?flextable::color error!!
   
  nr <- dim(x0)[1L]
  
  border_hard_ <- fp_border(
    width = 2 * init_flextable_defaults()$border.width, # *looks* like default border width used in ?flextable::flextable
    color = init_flextable_defaults()$border.color # '#666666', i.e., 'gray40'
  )

  x1 |> 
    flextable() |> 
    autofit(part = 'all') |>
    hline(i = seq_len(nr - 1L)) |> # `-1`: do not overwrite default bottom
    color(j = v_hue, color = hue_color, part = 'all') |>
    #bold(j = v_hue, part = 'all') |> # removed 2025-07-31
    
    add_header_row(values = c(' ', group), colwidths = c(1, nc), top = TRUE) |>
    add_header_row(values = c(' ', dnm), colwidths = c(1, nc), top = TRUE) |>
    add_footer_row(values = c(' ', group), colwidths = c(1, nc), top = FALSE) |>
    hline_bottom(border = border_hard_, part = 'all') |> # to make sure each row of footer has a bottom
    add_footer_row(values = c(' ', dnm), colwidths = c(1, nc), top = FALSE) |>
    hline_bottom(border = border_hard_, part = 'all') |>
    
    # [vline]: must be after adding all 'footer' !
    vline(j = v_hard, border = border_hard_, part = 'all') |>
    vline(j = v_soft, part = 'all') |>
  
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



# ?base::print
#' @title Print [DemographicTable]
#' 
#' @param x a [DemographicTable]
#' 
#' @param ... place holder for `S3` method dispatch
#' 
#' @returns 
#' Function [print.DemographicTable()] returns a \link[flextable]{flextable} \link[base]{invisible}-y.
#' 
#' @keywords internal
#' @export print.DemographicTable
#' @export
print.DemographicTable <- function(x, ...) {
  z <- x |> 
    as_flextable.DemographicTable(...) 
  print(z) # ?flextable:::print.flextable()
  return(invisible(z))
}

# @export print.sumtab
#' @export
print.sumtab <- function(x, ...) {
  x |>
    as_flextable.sumtab(...) |> 
    print()
}
