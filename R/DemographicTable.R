

#' @title Demographic Table
#' 
#' @description 
#' To create a demographic table with simple summary statistics, 
#' with optional comparison(s) over one or more groups.
#' 
#' @param data a \link[base]{data.frame}
#' 
#' @param data.name \link[base]{character} scalar, 
#' or the argument \link[base]{call} of `data`.  
#' A user-friendly name of the input `data`.
#' 
#' @param by one-sided \link[stats]{formula}, 
#' the name(s) of sub-group(s) for which the summary statistics are provided.
#' Default `NULL` indicating no sub-groups.
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @details 
#' 
#' A demographic table with simple summary statistics, with optional comparison(s) over one or more groups, is created.
#' 
#' \link[base]{numeric} variables are summarized in means, standard deviations, medians, inter-quartile-ranges (IQR), 
#' skewness, \eqn{p}-value of Shapiro-Wilk normality test and ranges.
#' If `group` is specified, they are compared using two-sample \link[stats]{t.test}, 
#' \link[stats]{wilcox.test} (Wilcoxon / Mann-Whitney), one-way \link[stats]{aov} (ANOVA) and/or 
#' \link[stats]{kruskal.test} (Kruskal-Wallis).
#' 
#' \link[base]{logical} and \link[base]{factor} variables are summarized in counts and percentages.
#' If `group` is specified, they are compared using \link[stats]{prop.test} (chi-squared)
#' and/or \link[stats]{fisher.test} (Fisher's exact).
#' 
#' @returns 
#' 
#' The function [DemographicTable()] returns an object of S3 class `'DemographicTable'`, 
#' which is a \link[stats]{listof} `'sumtab'` elements.
#' 
#' @keywords internal
#' @export
DemographicTable <- function(
    data, data.name = substitute(data), 
    by = NULL,
    ...
) {
  
  force(data.name)
  if (is.character(data.name)) {
    if (length(data.name) != 1L || anyNA(data.name) || !nzchar(data.name)) stop('illegal data.name')
  } else data.name <- deparse1(data.name)
  
  if (!is.data.frame(data)) stop('input must be data.frame')
  data <- as.data.frame(data) # use S3
  if (anyDuplicated.default(names(data))) stop('Duplicated column names in raw data?')
  data <- data[!vapply(data, FUN = \(i) all(is.na(i)), FUN.VALUE = NA)] # remove all-missing columns 
  for (i in seq_along(data)) {
    if (is.character(data[[i]])) data[[i]] <- factor(data[[i]])
  }
  
  if (!missing(by) && length(by)) {
    if (!is.call(by) || by[[1L]] != '~' || length(by) != 2L) stop('`by` must be one-sided formula')
    f <- by |> 
      terms.formula() |>
      attr(which = 'term.labels', exact = TRUE) |> # beautiful!
      lapply(FUN = \(i) {
        f <- i |>
          sprintf(fmt = '~%s') |>
          str2lang() |>
          model.frame.default(formula = _, data = data, na.action = NULL) |> # beautiful!
          interaction(drop = TRUE, sep = '.', lex.order = TRUE) # 'factor' :)
        
        nm <- i
        if (any(id <- is.na(f))) {
          nm <- sprintf(fmt = '%s\nn=%d (%.1f%%) missing', nm, sum(id), 1e2*mean.default(id))
        } # else do nothing
        
        if (length(unique(f[!id])) == 1L) {
          message('Group ', sQuote(i), ' has single value, thus removed.')
          return(invisible())
        }
        list(f) |>
          setNames(nm = nm)
      }) |>
      unlist(recursive = FALSE) # a 'list' of 'factor's; beautiful!!!
  } else f <- NULL

  
  ############################################
  ## Inspect variables (on the rows)
  ############################################
  
  cl1 <- data |>
    vapply(FUN = \(x) class(x)[1L], FUN.VALUE = '', USE.NAMES = TRUE)
  vlst <- split.default(names(cl1), f = factor(cl1))
  
  if (length(vlst$matrix)) {
    .Deprecated(msg = 'no longer support `matrix`-column')
    vlst$matrix <- NULL
  }
  
  ######################
  # Done! use `data`, `vlst` and `f` below
  ######################
  
  # 'overall' column
  ret0 <- .sumtab(data, vlst = vlst, ...)
  attr(ret0, which = 'data.name') <- data.name
  
  ret1 <- if (length(f)) {
    mapply(FUN = \(f, nm) {
      z <- .sumtab_by(data = data, f = f, vlst = vlst, ...)
      attr(z, which = 'group') <- nm
      attr(z, which = 'data.name') <- data.name
      return(z)
    }, f = f, nm = names(f), SIMPLIFY = FALSE, USE.NAMES = FALSE)
  } # else NULL
  
  ret <- c(list(ret0), ret1)
  class(ret) <- c('DemographicTable', 'listof')
  return(ret)
  
}






#' @export
c.DemographicTable <- function(...) {
  ret <- list(...) |>
    lapply(FUN = unclass) |> # a 'list' of 'list' (not 'listof')
    do.call(what = c)
  class(ret) <- c('DemographicTable', 'listof')
  return(ret)
}





# @param font.size \link[base]{numeric} scalar, see functions 
# \link[flextable]{set_flextable_defaults} and 
# \link[flextable]{init_flextable_defaults}
#' @importFrom officer fp_border
#' @importFrom scales pal_hue
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



#' @export
print.DemographicTable <- function(x, ...) {
  
  z <- x |> 
    as_flextable.DemographicTable(...) 
  
  # ?flextable:::print.flextable # read inside very carefully!!!
  z |> 
    print(...) 
  # rmarkdown code-chunk is `!interactive()` 
  
  return(invisible(z))
  
}



