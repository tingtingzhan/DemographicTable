

#' @title Convert [DemographicTable] to \link[flextable]{flextable}
#' 
#' @description 
#' Convert a [DemographicTable] to a \link[flextable]{flextable} object.
#' 
#' @param x a [DemographicTable] object
#' 
#' @param font.size \link[base]{numeric} scalar, see functions 
#' \link[flextable]{set_flextable_defaults} and 
#' \link[flextable]{init_flextable_defaults}
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
#' @importFrom officer fp_border
#' @importFrom scales pal_hue
#' @export as_flextable.DemographicTable
#' @export
as_flextable.DemographicTable <- function(x, font.size = 10, ...) {
  
  rnm <- x |>
    lapply(FUN = rownames)
  if (!all(duplicated(rnm)[-1L])) stop('rownames not all-same')
  
  dnm <- x |>
    vapply(FUN = attr, which = 'data.name', exact = TRUE, FUN.VALUE = '')
  group <- x |>  
    vapply(FUN = attr, which = 'group', exact = TRUE, FUN.VALUE = '')
  compare <- x |>
    vapply(FUN = attr, which = 'compare', exact = TRUE, FUN.VALUE = NA)
  nc <- x |>
    vapply(FUN = ncol, FUN.VALUE = NA_integer_)
  
  x0 <- do.call(what = cbind, args = x)
  x1 <- data.frame(' ' = dimnames(x0)[[1L]], unclass(x0), row.names = NULL, check.names = FALSE, fix.empty.names = FALSE, stringsAsFactors = FALSE)
  xnm <- names(x1)
  if (sum(sig_id <- (xnm == 'Signif')) > 1L) {
    xnm[sig_id] <- paste0('Signif', seq_len(sum(sig_id)))
  }
  names(x1) <- xnm
  
  v_hard <- c(1L, 1L + cumsum(nc[-length(nc)]))
  #v_soft <- setdiff(seq_len(sum(nc)), v_hard)
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
  
  set_flextable_defaults(font.size = font.size)
  
  on.exit(init_flextable_defaults())
  
  x1 |> 
    flextable() |> 
    autofit(part = 'all') |>
    border_inner(part = 'all') |> 
    color(j = v_hue, color = hue_color, part = 'all') |>
    #bold(j = v_hue, part = 'all') |> # removed 2025-07-31
    
    add_header_row(values = c(' ', group), colwidths = c(1, nc), top = TRUE) |>
    add_header_row(values = c(' ', dnm), colwidths = c(1, nc), top = TRUE) |>
    add_footer_row(values = c(' ', group), colwidths = c(1, nc), top = FALSE) |>
    hline_bottom(border = .border_hard(), part = 'all') |> # to make sure each row of footer has a bottom
    add_footer_row(values = c(' ', dnm), colwidths = c(1, nc), top = FALSE) |>
    hline_bottom(border = .border_hard(), part = 'all') |>
    
    # [vline]: must be after adding all 'footer' !
    vline(j = v_hard, border = .border_hard(), part = 'all') |>
  
    merge_h(part = 'header') |>
    merge_v(part = 'header') |>
    merge_h(part = 'footer') |>
    merge_v(part = 'footer') |>
    color(i = 1:2, color = 'black', part = 'header') |> # having `v_hue` or not
    align(align = 'center', part = 'header') |>
    align(align = 'center', part = 'footer')
    
}







# @export as_flextable.sumtab
#' @export
as_flextable.sumtab <- function(x, ...) {
  
  x1 <- data.frame(
    ' ' = dimnames(x)[[1L]], 
    unclass(x), 
    row.names = NULL, check.names = FALSE, fix.empty.names = FALSE, stringsAsFactors = FALSE
  )
  dnm <- attr(x, which = 'data.name', exact = TRUE)
  names(x1)[1L] <- dnm
  
  ret0 <- x1 |>
    flextable() |> 
    autofit(part = 'all') |>
    border_inner(part = 'all') |>
    vline(j = 1L, border = .border_hard(), part = 'all')
  
  group <- attr(x, which = 'group', exact = TRUE)
  if (!nzchar(group)) return(ret0)
   
  nc <- ncol(x)
  compare <- x |>
    attr(which = 'compare', exact = TRUE)
  ncolor <- if (compare) nc - 1L else nc
  ret0 |> 
    color(j = seq_len(ncolor) + 1L, color = pal_hue()(n = ncolor), part = 'all') |>
    add_header_row(values = c(dnm, group), colwidths = c(1, nc), top = TRUE) |>
    color(i = 1, color = 'black', part = 'header') |>
    align(i = 1L, j = NULL, align = 'center', part = 'header') |>
    merge_v(part = 'header')
  
}





# hard border
.border_hard <- function() {
  fp_border(
    width = 2 * init_flextable_defaults()$border.width, # *looks* like default border width used in ?flextable::flextable
    color = init_flextable_defaults()$border.color # '#666666', i.e., 'gray40'
  )
}







# ?base::print
#' @title Print [DemographicTable]
#' 
#' @param x a [DemographicTable]
#' 
#' @param ... additional parameters of function \link[flextable]{print.flextable}
#' 
#' @returns 
#' Function [print.DemographicTable()] returns a \link[flextable]{flextable} \link[base]{invisible}-y.
#' 
#' @keywords internal
# @importFrom utils getS3method
#' @export print.DemographicTable
#' @export
print.DemographicTable <- function(x, ...) {
  
  z <- x |> 
    as_flextable.DemographicTable(...) 
  
  # ?flextable:::print.flextable # read inside very carefully!!!
  z |> 
    print(...) 
  # rmarkdown code-chunk is `!interactive()` 
  
  return(invisible(z)) # does not work in vignette ..
  
}

# @export print.sumtab
#' @export
print.sumtab <- function(x, ...) {
  x |>
    as_flextable.sumtab(...) |> 
    print() # ?flextable:::print.flextable()
}
