

#' @title Create Demographic Table
#' 
#' @description Create a demographic table with simple summary statistics, with optional comparison(s) over one or more groups.
#' 
#' @param data a \link[base]{data.frame}
#' 
#' @param data.name \link[base]{character} scalar, or the argument call of `data`.  
#' A user-friendly name of the input `data`.
#' 
#' @param groups \link[base]{character} scalar or \link[base]{vector}, 
#' the name(s) of sub-group(s) for which the summary statistics are to be provided.
#' Default `NULL` indicating no sub-groups.
#' 
#' @param keep_missing_group \link[base]{logical} scalar.
#' If `TRUE` (default), the subjects with missing `group`
#' are put into a new group (`'.missing'`).
#' if `FALSE`, these subjects are removed from group-wise summary statistics.
#' 
#' @param exclude \link[base]{character} \link[base]{vector}, 
#' the name(s) of variable(s) to be excluded.  
#' Default `NULL` indicating no variable are to be excluded.
#' 
#' @param exclude_pattern (optional) \link[base]{character} scalar as 
#' \link[base]{regex} (regular expression), 
#' the pattern of the names of the variable(s) to be excluded. 
#' 
#' @param include \link[base]{character} \link[base]{vector}, 
#' the name(s) of variable(s) to be included.
#' Default `names(data)` indicating all variables are to be included.
#' 
#' @param include_pattern (optional) \link[base]{character} scalar as 
#' \link[base]{regex} (regular expression), 
#' the pattern of the names of the variable(s) to be included.
#' 
#' @param paired \link[base]{logical} scalar, whether to perform paired test (default `FALSE`)
#' 
#' @param robust \link[base]{logical} scalar. 
#' If `TRUE` (default), use non-parametric methods for 
#' non-normally distributed \link[base]{numeric} variables.
#' 
#' @param overall \link[base]{logical} scalar.
#' If `TRUE` (default), a column of overall summary statistics will be provided.
#' 
#' @param compare \link[base]{logical} scalar.
#' If `TRUE` (default), comparisons between group(s) will be made.
#' 
#' @param pairwise \link[base]{integer} scalar,
#' minimum number of groups where pairwise comparisons need to be performed.
#' Default `3L`.
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
#' Function [DemographicTable] returns an object of S3 class `'DemographicTable'`, 
#' which inherits from \link[base]{matrix}.
#' 
#' @importFrom stats aov chisq.test fisher.test kruskal.test mcnemar.test pairwise.prop.test pairwise.t.test pairwise.wilcox.test prop.test quantile t.test var wilcox.test
#' 
#' @examples 
#' DemographicTable(esoph)
#' DemographicTable(ToothGrowth, groups = 'supp', include = 'len')
#' DemographicTable(ToothGrowth, groups = 'supp', include = 'len', paired = TRUE)
#' DemographicTable(ToothGrowth, groups = 'supp', include = 'len', compare = FALSE)
#' DemographicTable(warpbreaks, groups = c('wool', 'tension'))
#' DemographicTable(mtcars, groups = c('vs', 'am'), include = c('mpg', 'cyl', 'disp'))
#' 
#' # with missing value
#' DemographicTable(airquality, groups = 'Month', exclude = 'Day')
#' DemographicTable(MASS::survey, groups = 'Smoke', keep_missing_group = FALSE)
#' DemographicTable(MASS::survey, groups = 'Smoke', keep_missing_group = FALSE, useNA = 'always')
#' 
#' # write to Word file
#' library(flextable)
#' library(officer)
#' x = read_docx() |> body_add_flextable(value = as_flextable(DemographicTable(esoph)))
#' (out = file.path(tempdir(), 'demotable.docx'))
#' print(x, target = out)
#' # system(paste('open', out)) # works on Mac & Windows, but requires Microsoft Word
#' file.remove(out)
#' 
#' @name DemographicTable
#' @export
DemographicTable <- function(data, ...) UseMethod('DemographicTable')

#' @rdname DemographicTable
#' @method DemographicTable data.frame
#' @export DemographicTable.data.frame
#' @export
DemographicTable.data.frame <- function(
    data, data.name = substitute(data), 
    groups = NULL, keep_missing_group = TRUE,
    exclude = NULL, exclude_pattern, 
    include, include_pattern, 
    paired = FALSE,
    robust = TRUE,
    overall = TRUE, 
    compare = TRUE,
    pairwise = 3L,
    ...
) {
  
  force(data.name)
  
  if (!is.data.frame(data)) stop('input must be data.frame')
  data <- as.data.frame(data) # use S3
  if (anyDuplicated.default(names(data))) stop('Duplicated column names in raw data?')
  data <- data[!vapply(data, FUN = function(i) all(is.na(i)), FUN.VALUE = NA)] # remove all-missing columns 
  
  if (length(groups)) {
    if (!is.character(groups) || anyNA(groups) || !all(nzchar(groups))) stop('groups must be character without NA or zchar')
    groups <- unique.default(groups)
    if (any(id <- is.na(match(groups, table = names(data), nomatch = NA_integer_)))) stop(sQuote(groups[id]), ' not in names of data. Removed accidentally?')
    if (any(id <- vapply(data[groups], FUN = is.matrix, FUN.VALUE = NA, USE.NAMES = FALSE))) stop(sQuote(groups[id]), ' is/are matrix column(s).')
  }
  
  if (!missing(exclude_pattern)) {
    exclude <- unique.default(c(exclude, grep(exclude_pattern, x = names(data), value = TRUE)))
  }
  
  include <- if (missing(include_pattern)) {
    if (missing(include)) names(data) else include
  } else {
    ptn_include <- grep(include_pattern, x = names(data), value = TRUE)
    if (missing(include)) ptn_include else unique.default(c(include, ptn_include))
  }
  
  #include <- setdiff(x = include, y = c(exclude, groups)) # made sure `include` and `groups` has no overlap
  include <- setdiff(x = include, y = exclude)
  if (!length(include)) stop('length-0 `include`: Nothing on the rows of demographic table')
  
  rm(exclude)
  
  if (any(id <- is.na(match(include, table = names(data), nomatch = NA_integer_)))) {
    message('Unknown variable(s): ', sQuote(include[id]), ' removed.')
    include <- include[!id]
  }
  
  data <- data[c(include, groups)]
  
  for (i in include) {
    if (is.character(data[[i]])) data[[i]] <- factor(data[[i]]) 
    # MUST!! otherwise missing groups in subset-data will not print zero-count
  }
  
  ##################################################################
  ## Inspect `groups` in detail (removing rows if needed)
  ##################################################################
  
  if (length(groups)) {
    
    if (keep_missing_group) {
      for (ig in groups) {
        if (any(id <- is.na(data[[ig]]))) {
          data[[ig]] <- as.character(data[[ig]])
          data[[ig]][id] <- '.missing'
        }# else do nothing
      }
    } # else do nothing!!!!
    
    for (ig in groups) {
      igv <- data[[ig]]
      if (length(unique(igv[!is.na(igv)])) == 1L) {
        message('Column ', sQuote(ig), ' has single value, thus removed from `groups`.')
        groups <- setdiff(groups, ig)
      }
    } # remove any group with all-same entries
    
  }
  
  ############################################
  ## Inspect `include` in detail
  ############################################
  
  vlst <- class1List(data[include]) # without `groups`
  
  if (length(vlst$matrix)) {
    mtype <- vapply(data[vlst$matrix], FUN = typeof, FUN.VALUE = '')
    if (any(id_double <- (mtype == 'double'))) {
      vlst$difftime <- c(vlst$difftime, vlst$matrix[id_double][id_difftime <- vapply(data[vlst$matrix[id_double]], FUN = inherits, what = 'difftime', FUN.VALUE = NA)])
      vlst$numeric <- c(vlst$numeric, vlst$matrix[id_double][!id_difftime])
    }
    # c('factor', 'matrix') is in `vlst$factor` already...
    if (any(id_bool <- (mtype == 'logical'))) {
      vlst$logical <- c(vlst$logical, vlst$matrix[id_bool])
    }
    if (any(!id_bool & !id_double)) stop('uncovered matrix column')
  }
  
  ######################
  # Done! use `data`, `vlst` and `groups` below
  ######################
  
  overall <- overall & !paired
  
  ret <- if (overall) DemographicSummaries(data, vlst = vlst, ...) # else NULL      
  
  if (length(groups)) {
    ret_by <- lapply(groups, FUN = demoTab_by, data = data, vlst = vlst, compare = compare, paired = paired, robust = robust, pairwise = pairwise, ...)
    rets <- if (length(ret)) c(list(ret), ret_by) else ret_by
    # is_equal(rets, FUN = function(x) dimnames(x)[[1L]])
    ret <- do.call(cbind, args = rets)
    attr(ret, which = 'groups') <- groups
    attr(ret, which = 'test') <- unique.default(unlist(lapply(ret_by, FUN = attr, which = 'test', exact = TRUE), use.names = FALSE))
    attr(ret, which = 'ncols') <- vapply(rets, FUN = function(x) dim(x)[2L], FUN.VALUE = 0L)
  } else if (!overall) {
    stop('must do at least `overall` or by-`groups`')
  }

  attr(ret, which = 'data.name') <- if (is.character(data.name)) {
    if (length(data.name) != 1L || anyNA(data.name) || !nzchar(data.name)) stop('illegal data.name')
    data.name
  } else deparse1(data.name)
  class(ret) <- c('DemographicTable', class(ret))
  return(ret)
  
}





##################
## work horse
##################

DemographicSummaries <- function(data, vlst, fmt = '%.1f', ...) {# useNA = c('no', 'always'), 
  
  #useNA <- match.arg(useNA) # 'ifany' is not allowed
  
  out_num <- if (length(.num <- c(vlst$integer, vlst$numeric))) {
    names(.num) <- .num
    unlist(lapply(data[.num], FUN = summaryText.default, fmt = fmt, ...), use.names = TRUE)
  } #else NULL
  
  out_difft <- if (length(.difft <- vlst$difftime)) {
    d_difft <- data[.difft]
    names(d_difft) <- paste0(.difft, ' (', vapply(data[.difft], FUN = attr, which = 'units', exact = TRUE, FUN.VALUE = ''), ')') # ?base::units.difftime
    unlist(lapply(d_difft, FUN = summaryText.default, fmt = fmt, ...), use.names = TRUE)
  } #else NULL
  
  out_bool <- if (length(.bool <- vlst$logical)) {
    d_bool <- data[.bool]
    names(d_bool) <- paste0(.bool, ': n (%)')
    vapply(d_bool, FUN = summaryText.logical, fmt = fmt, ..., FUN.VALUE = '')
  } #else NULL
  
  out_factor <- if (length(.fact <- c(vlst$character, vlst$factor, vlst$ordered))) {
    d_fact <- data[.fact]
    names(d_fact) <- paste0(.fact, ': n (%)')
    unlist(lapply(d_fact, FUN = summaryText, fmt = fmt, ...), use.names = TRUE) # useNA = useNA, 
  } #else NULL

  ret <- c(out_num, out_difft, out_bool, out_factor)
  array(ret, dim = c(length(ret), 1L), 
        dimnames = list(names(ret), paste0('N=', .row_names_info(data, type = 2L))))
   
}




demoTab_by <- function(data, vlst, group, robust = TRUE, compare = TRUE, paired = FALSE, pairwise = 3L, ...) { # SMD = FALSE, 
  
  if (!is.character(group) || length(group) != 1L || anyNA(group) || !nzchar(group)) stop('`group` must be len-1 character')
  
  fgrp <- factor(data[[group]])
  gidx <- split.default(seq_along(fgrp), f = fgrp)
  gN <- lengths(gidx, use.names = FALSE)
  
  ret <- do.call(cbind, args = lapply(gidx, FUN = function(id) { # (id = gidx[[1L]])
    DemographicSummaries(data[id, , drop = FALSE], vlst = vlst, ...)
  }))
  colnames(ret) <- if (!paired) {
    sprintf(fmt = '%s\n= %s\nN=%d (%.1f%%)', group, names(gidx), gN, 1e2*gN/sum(gN))
  } else sprintf(fmt = '%s\n= %s\nN=%d', group, names(gidx), gN)
  
  # removing single 'group' for p-values
  txt_g1 <- if (any(g1 <- (gN == 1L))) {
    gidx <- gidx[-g1]
    paste0('(', sum(g1), ' ', sQuote(group), ' level(s) of\n single obs omitted)')
  } # else NULL
  
  ng <- length(gidx)
  if (ng < 2L) return(ret)
  
  if (compare) {
    p_double <- vapply(c(vlst$integer, vlst$numeric, vlst$difftime), FUN = function(i) compare_double(demo_get(x = data[[i]], gidx = gidx), paired = paired, robust = robust, pairwise = pairwise, ...), FUN.VALUE = '')
    p_bool <- vapply(vlst$logical, FUN = function(i) compare_bool(demo_get(x = data[[i]], gidx = gidx), paired = paired, pairwise = pairwise, ...), FUN.VALUE = '')
    p_factor <- vapply(c(vlst$character, vlst$factor, vlst$ordered), FUN = function(i) compare_factor(x = data[[i]], g = fgrp, paired = paired, ...), FUN.VALUE = '')
    pval <- c(p_double, p_bool, p_factor)
    if (dim(ret)[1L] != length(pval)) stop('demographic table contruction wrong: pval do not match summary stats')
    ret_compare <- as.matrix(pval)
    colnames(ret_compare) <- paste0('Significance\n(by ', group, ')\n', txt_g1)
  } else ret_compare <- NULL
  
  ret <- cbind(ret, ret_compare)
  return(ret)

}







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
#' @importFrom flextable as_flextable flextable autofit hline vline
#' @export as_flextable.DemographicTable
#' @export
as_flextable.DemographicTable <- function(x, ...) {
  x1 <- data.frame(' ' = dimnames(x)[[1L]], unclass(x), row.names = NULL, check.names = FALSE, fix.empty.names = FALSE, stringsAsFactors = FALSE)
  names(x1)[1L] <- attr(x, which = 'data.name', exact = TRUE)
  
  y0 <- autofit(flextable(data = x1), part = 'all')
  y1 <- hline(y0, i = seq_len(dim(x)[1L] - 1L))
  nc <- attr(x, which = 'ncols', exact = TRUE)
  ret <- vline(y1, j = c(1L, 1L + cumsum(nc[-length(nc)])))
  return(ret) 
}


# ?base::print
#' @export
print.DemographicTable <- function(x, ...) print(as_flextable.DemographicTable(x, ...))



#' @title Write [DemographicTable] to LaTeX
#' 
#' @description Write [DemographicTable] to LaTeX.
#' 
#' @param x a [DemographicTable] object
#' 
#' @param ... additional parameters of \link[xtable]{xtable}
#' 
#' @returns 
#' 
#' Function [xtable.DemographicTable] returns an \link[xtable]{xtable} object.
#' 
#' @examples 
#' (tb = DemographicTable(ToothGrowth, groups = 'supp'))
#' library(xtable)
#' print(xtable(tb), sanitize.text.function = identity, 
#'  sanitize.colnames.function = NULL, include.rownames = FALSE)
#' 
#' @importFrom xtable xtable
#' @export xtable.DemographicTable
#' @export
xtable.DemographicTable <- function(x, ...) {
  row_break <- function(x) {
    # `x` is row-1 matrix
    x0 <- c(rownames(x), x)
    x0 <- gsub('\u00B1', replacement = '\\\\pm ', x = x0)
    x0 <- gsub('\u2713', replacement = '\\\\checkmark ', x = x0)
    x0 <- gsub('\u26A0', replacement = '\\\\times ', x = x0)
    y0 <- strsplit(x0, split = '\n')
    ny <- lengths(y0, use.names = FALSE)
    n <- max(ny)
    y1 <- lapply(y0, FUN = function(i) c(i, rep('', times = n - length(i))))
    do.call(cbind, args = y1)
  }
  
  y0 <- do.call(rbind, args = lapply(seq_len(dim(x)[1L]), FUN = function(i) {
    row_break(x[i, , drop = FALSE])
  }))
  cnm <- gsub(pattern = '\\n', replacement = ' ', dimnames(unclass(x))[[2L]])
  colnames(y0) <- c(attr(x, which = 'data.name', exact = TRUE), cnm)
  
  y1 <- as.data.frame.matrix(y0, make.names = FALSE, row.names = FALSE)
  return(xtable(y1, ...))
}





demo_get <- function(x, gidx) {
  # `x`: 'double', 'logical' or 'factor' responses to be compared
  # `gidx`: a 'list' of group indices
  xm <- is.matrix(x)
  xs <- lapply(gidx, FUN = function(i) {
    y <- unclass(if (xm) c(x[i, ]) else x[i])
    #y[!is.na(y)] # do NOT do this, for `paired` test
    if (all(is.na(y))) return(NULL) # remove all-NA elements
    return(y)
  })
  return(xs[lengths(xs, use.names = FALSE) > 1L])
}




symb <- function(p) { # vectorized
  ret <- character(length = length(p))
  ret[p < .05] <- '\u2605 '
  return(ret)
}


pText_pairwise.htest <- function(x) {
  dnm <- dimnames(pv0 <- x$p.value)
  id <- lower.tri(pv0, diag = TRUE)
  pv <- pv0[id]
  pnm <- outer(dnm[[1L]], dnm[[2L]], FUN = paste, sep = ' vs. ')[id]
  sprintf(fmt = paste0(symb(pv), '%.3f (%s)'), pv, pnm)
}



# @param pairwise \link[base]{integer} scalar, the maximum group number under which pairwise tests,
# \link[stats]{pairwise.t.test} and \link[stats]{pairwise.wilcox.test}, are preferred.  Default value `3L`.
compare_double <- function(xs, CLT = TRUE, robust = TRUE, pairwise = 3L, alternative = c('two.sided', 'less', 'greater'), paired = FALSE, ...) {
  
  ng <- length(xs)
  if (ng <= 1L) return('1 arm or less')
  if (paired) {
    if (all(duplicated.default(xs)[-1L])) return('') # e.g., paired differences all zero
    #if (!all(duplicated.default(lengths(xs, use.names = FALSE))[-1L])) stop('Sample size per group must be all-same for paired test')
  }

  alternative <- match.arg(alternative)
  
  p_shapiro <- if (!robust) 1 else vapply(xs, FUN = pval_shapiro, CLT = CLT, FUN.VALUE = 0, USE.NAMES = FALSE)
    
  if (ng == 2L) { # ?stats::t.test or ?stats::wilcox.test
    # always `2L` vs. `1L` (column 1 as reference)
    if (any(p_shapiro < .05)) {
      test <- tryCatch(suppressWarnings(wilcox.test(x = xs[[2L]], y = xs[[1L]], paired = paired, exact = FALSE, conf.int = TRUE, alternative = alternative)), error = identity)
      if (inherits(test, what = 'error')) return('')
      est <- test$estimate # whether `paired` or not
      fmt <- paste0('Median diff: %.3f\n95%% CI (%.3f, %.3f)\np = %.3f', symb(test$p.value), '\n', if (paired) 'Paired ', 'Wilcoxon-\nMann-Whitney')
    } else {
      test <- t.test(x = xs[[2L]], y = xs[[1L]], paired = paired, alternative = alternative)
      if (is.na(test$statistic)) return('') # very likely identical(x, y) in ?stats::t.test
      est <- if (paired) test$estimate else test$estimate[1L] - test$estimate[2L]
      fmt <- paste0('Mean diff: %.3f\n95%% CI (%.3f, %.3f)\np = %.3f', symb(test$p.value), '\n', if (paired) 'Paired' else 'Two-Sample', ' t')
    }
    return(sprintf(fmt = fmt, est, test$conf.int[1L], test$conf.int[2L], test$p.value))
  }

  if (!is.numeric(pairwise) || length(pairwise) != 1L || anyNA(pairwise) || pairwise < 2L) stop('illegal `pairwise`')
  # `is.numeric(pairwise)` not `is.integer(pairwise)` to allow Inf
  
  x <- unlist(xs, use.names = FALSE)
  g <- rep(names(xs), times = lengths(xs, use.names = FALSE))
  
  if (any(p_shapiro < .05)) {
    if (ng <= pairwise) {
      suppressWarnings(tmp <- pairwise.wilcox.test(x = x, g = g, p.adjust.method = 'none', paired = paired, alternative = alternative))
      # ?stats::pairwise.wilcox.test only provides p-value, not confidence intervals
      return(paste(c(pText_pairwise.htest(tmp), paste0('Pairwise ', if (paired) 'Paired\n', 'Wilcoxon-Mann-Whitney')), collapse = '\n'))
    }
    
    if (paired) stop('Paired Kruskal-Wallis test?')
    return(tryCatch(expr = {
      p.value <- kruskal.test(x = x, g = g, ...)$p.value
      sprintf(fmt = paste0(symb(p.value), '%.3f\nKruskal-Wallis'), p.value)
    }, error = 'Kruskal-Wallis test\nnot available'))
  }
  
  if (ng <= pairwise) {
    tmp <- pairwise.t.test(x = x, g = g, pool.sd = FALSE, paired = paired, p.adjust.method = 'none', alternative = alternative)
    return(paste(c(pText_pairwise.htest(tmp), paste0('Pairwise ', if (paired) 'Paired\n', 'Two-Sample t')), collapse = '\n'))
  }
  
  # ?stats::aov requires formula~data parameterization
  if (paired) stop('Paired ANOVA; not supported yet')
  return(tryCatch(expr = {
    p.value <- summary(aov(x ~ g))[[1L]][1L, 'Pr(>F)']
    sprintf(fmt = paste0(symb(p.value), '%.3f\nANOVA'), p.value)
  }, error = 'ANOVA not available'))
}




compare_bool <- function(xs, pairwise = 3L, alternative = c('two.sided', 'less', 'greater'), paired = FALSE, ...) {
  
  ng <- length(xs)
  if (ng <= 1L) return('1 arm or less')
  if (paired) {
    if (all(duplicated.default(xs)[-1L])) return('') # e.g., variables not changing across time
    if (!all(duplicated.default(lengths(xs, use.names = FALSE))[-1L])) stop('Sample size per group must be all-same for paired test')
  }
  
  alternative <- match.arg(alternative)
  
  xs0 <- lapply(xs, FUN = function(x) x[!is.na(x)]) # must save `xs` for 'paired = TRUE'
  X <- vapply(xs0, FUN = sum, FUN.VALUE = 0L, USE.NAMES = TRUE)
  N <- lengths(xs0, use.names = TRUE)
  fish <- tryCatch(fisher.test(cbind(X, N-X), alternative = alternative), error = function(e) {
    if (grepl('consider using \'simulate.p.value=TRUE\'$', e$message)) {
      fisher.test(cbind(X, N-X), alternative = alternative, simulate.p.value = TRUE)
    } else stop(e$message)
  })
  fisher_txt <- sprintf(fmt = paste0(symb(fish$p.value), '%.3f\nFisher\'s Exact'), fish$p.value)
  
  if (ng == 2L) {
    
    if (paired) {
      # McNemar test not available for degenerated 2*2 table
      x1 <- xs[[1L]]
      x2 <- xs[[2L]]
      ok <- (!is.na(x1) & !is.na(x2))
      x10 <- x1[ok]
      x20 <- x2[ok]
      if (all(x10) || !any(x10) || all(x20) || !any(x20)) return('')
      if (all(x10 == x20)) return('All non-missing pairs identical')
      mcnemar <- tryCatch(mcnemar.test(x = x10, y = x20), error = identity)
      if (inherits(mcnemar, 'error')) {
        #print(freqs(x10))
        #print(freqs(x20))
        stop('here')
      }
      return(sprintf(fmt = paste0(symb(mcnemar$p.value), '%.3f\nMcNemar\'s'), mcnemar$p.value))
    }
    
    if (any(X == 0L, X == N)) return('') # p-value means nothing
    return(tryCatch(expr = {
      #p.value <- binom.test(x = X, n = N, alternative = alternative)$p.value
      #sprintf(fmt = paste0(symb(p.value), '%.3f\nExact Binomial'), p.value) # sometimes looks wrong..
      p.value <- prop.test(x = X, n = N, alternative = alternative)$p.value
      sprintf(fmt = paste0(symb(p.value), '%.3f\n\u03C7\u00B2 (chi-square)'), p.value)
    }, warning = function(w) fisher_txt))
  }
  
  if (ng <= pairwise) {
    if (paired) stop('Paired McNemar test, not ready yet')
    tmp <- suppressWarnings(pairwise.prop.test(x = X, n = N, p.adjust.method = 'none', alternative = alternative))
    return(paste(c(pText_pairwise.htest(tmp), '\u03C7\u00B2'), collapse = '\n'))
  }
  
  if (paired) stop('not ready yet')
  return(fisher_txt)
  
}




compare_factor <- function(x, g, paired = FALSE, ...) {
  # will use ?stats::fisher.test or ?stats::chisq.test even if the factor has 2 levels (i.e. essentially binary)
  
  if (is.matrix(x)) g <- rep(g, times = dim(x)[2L]) # as of 2022-03-08, ?base::table will not recycle shorter argument
  tab <- table(x, g, useNA = 'no') # `x` can be either 'factor' or 'character'
  if (anyNA(tab)) stop('should not happen')
  if (any(dim(tab) == 1L)) return('') # no comparison should be given
  
  if (paired) {
    #xs <- lapply(seq_len(dim(tab)[2L]), FUN = function(i) tab[,i])
    #if (all(duplicated.default(xs)[-1L])) return('') # e.g., variables not changing across time
    #stop('not ready yet')
  }
  
  # ?stats::fisher.test removes all-0 rows or columns
  tmp <- tryCatch(fisher.test(tab), error = function(e) {
    tmp <- if (grepl('simulate.p.value=TRUE', x = e$message)) {
      tryCatch(fisher.test(tab, simulate.p.value = TRUE), error = identity, warning = identity)
    } else tryCatch(chisq.test(tab), error = identity, warning = identity)
    if (inherits(tmp, what = 'error')) return('Fisher\'s exact\nnor Chi2 test available')
    if (inherits(tmp, what = 'warning')) return(suppressWarnings(chisq.test(tab)))
    return(tmp)
  })
  
  if (is.character(tmp)) return(tmp)
  p.value <- tmp$p.value
  if (grepl('^Fisher', tmp$method)) return(sprintf(fmt = paste0(symb(p.value), '%.3f\nFisher\'s Exact'), p.value))
  return(sprintf(fmt = paste0(symb(p.value), '%.3f\n\u03C7\u00B2 (chi-square)'), p.value))
  
}




