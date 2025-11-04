

#' @title Create Demographic Table
#' 
#' @description Create a demographic table with simple summary statistics, with optional comparison(s) over one or more groups.
#' 
#' @param data a \link[base]{data.frame}
#' 
#' @param data.name \link[base]{character} scalar, or the argument \link[base]{call} of `data`.  
#' A user-friendly name of the input `data`.
#' 
#' @param by one-sided \link[stats]{formula}, 
#' the name(s) of sub-group(s) for which the summary statistics are provided.
#' Default `NULL` indicating no sub-groups.
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
#' Function [DemographicTable()] returns an object of S3 class `'DemographicTable'`, 
#' which is a \link[stats]{listof} `'sumtab'` elements.
#' 
#' @keywords internal
#' @export
DemographicTable <- function(
    data, data.name = substitute(data), 
    by = NULL,
    robust = TRUE,
    overall = TRUE, 
    compare = TRUE,
    pairwise = 3L,
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
  ## Inspect in detail
  ############################################
  
  # without `by`
  # .. copy tzh::class1List
  cl1 <- vapply(data, FUN = \(x) class(x)[1L], FUN.VALUE = '', USE.NAMES = TRUE)
  vlst <- split.default(names(cl1), f = factor(cl1))
  
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
    #if (any(!id_bool & !id_double)) stop('uncovered matrix column') # allow, just ignore these cases
  }
  
  ######################
  # Done! use `data`, `vlst` and `f` below
  ######################
  
  ret0 <- if (overall) list(.sumtab(data, data.name = data.name, vlst = vlst, ...)) # else NULL      
  ret1 <- if (length(f)) {
    f |>
      seq_along() |>
      lapply(FUN = \(i) {
        z <- .sumtab_by(data = data, data.name = data.name, f = f[[i]], vlst = vlst, compare = compare, robust = robust, pairwise = pairwise, ...)
        attr(z, which = 'group') <- setNames(nm = names(f)[i]) # downstream compatibility
        return(z)
      })
  }
  ret <- c(ret0, ret1)
  if (!length(ret)) stop('wont happen')
  
  class(ret) <- c('DemographicTable', 'listof')
  return(ret)
  
}










##################
## work horse
##################

.sumtab <- function(data, data.name, vlst, fmt = '%.1f', ...) {# useNA = c('no', 'always'), 
  
  out_num <- if (length(.num <- c(vlst$integer, vlst$numeric))) {
    names(.num) <- .num
    data[.num] |>
      lapply(FUN = .sumstat.default, fmt = fmt, ...) |>
      unlist(use.names = TRUE)
  } #else NULL
  
  out_difft <- if (length(.difft <- vlst$difftime)) {
    d_difft <- data[.difft]
    names(d_difft) <- paste0(.difft, ' (', vapply(data[.difft], FUN = attr, which = 'units', exact = TRUE, FUN.VALUE = ''), ')') # ?base::units.difftime
    d_difft |>
      lapply(FUN = .sumstat.default, fmt = fmt, ...) |>
      unlist(use.names = TRUE)
  } #else NULL
  
  out_bool <- if (length(.bool <- vlst$logical)) {
    d_bool <- data[.bool]
    names(d_bool) <- paste0(.bool, ': n (%)')
    d_bool |>
      vapply(FUN = .sumstat.logical, fmt = fmt, ..., FUN.VALUE = '')
  } #else NULL
  
  out_factor <- if (length(.fact <- c(vlst$character, vlst$factor, vlst$ordered))) {
    d_fact <- data[.fact]
    names(d_fact) <- paste0(.fact, ': n (%)')
    d_fact |>
      lapply(FUN = .sumstat, fmt = fmt, ...) |>
      unlist(use.names = TRUE) 
  } #else NULL

  ret0 <- c(out_num, out_difft, out_bool, out_factor)
  ret <- array(ret0, dim = c(length(ret0), 1L), dimnames = list(
    names(ret0), 
    paste0('n=', .row_names_info(data, type = 2L))
  ))
  attr(ret, which = 'data.name') <- data.name
  attr(ret, which = 'group') <- '' # important
  attr(ret, which = 'compare') <- FALSE
  #class(ret) <- c('sumtab', class(ret))
  class(ret) <- 'sumtab'
  return(ret)
}




#.sumtab_by <- function(data, data.name, vlst, group, robust = TRUE, compare = TRUE, pairwise = 3L, ...) { # SMD = FALSE,
.sumtab_by <- function(data, data.name, vlst, f, robust = TRUE, compare = TRUE, pairwise = 3L, ...) { # SMD = FALSE, 
  #if (!is.character(group) || length(group) != 1L || anyNA(group) || !nzchar(group)) stop('`group` must be len-1 character')
  
  #f <- factor(data[[group]])
  
  ######## parameter `f` is a 'factor' !!!
  
  gidx <- split.default(seq_along(f), f = f) # missingness in `f` dropped
  gN <- lengths(gidx, use.names = FALSE)
  
  ret <- gidx |>
    lapply(FUN = \(id) { # (id = gidx[[1L]])
      .sumtab(data[id, , drop = FALSE], data.name = '', vlst = vlst, ...)
    }) |>
    do.call(what = cbind)
  colnames(ret) <- sprintf(fmt = '%s\nn=%d (%.1f%%)', names(gidx), gN, 1e2*gN/length(f)) # before removing NA!!!
  
  # removing single 'group' for p-values
  txt_g1 <- if (any(g1 <- (gN == 1L))) {
    gidx <- gidx[!g1]
    #paste0('(', sum(g1), ' ', sQuote(group), ' level(s) of\n single obs omitted)')
    paste0('(', sum(g1), ' level(s) of\n single obs omitted)')
  } # else NULL
  
  ng <- length(gidx)
  if (ng < 2L) return(ret)
  
  if (compare) {
    p_double <- c(vlst$integer, vlst$numeric, vlst$difftime) |> vapply(FUN = \(i) compare_double(demo_get(x = data[[i]], gidx = gidx), robust = robust, pairwise = pairwise, ...), FUN.VALUE = '')
    p_bool <- vlst$logical |> vapply(FUN = \(i) compare_bool(demo_get(x = data[[i]], gidx = gidx), pairwise = pairwise, ...), FUN.VALUE = '')
    p_factor <- c(vlst$character, vlst$factor, vlst$ordered) |> vapply(FUN = \(i) compare_factor(x = data[[i]], g = f, ...), FUN.VALUE = '')
    pval <- c(p_double, p_bool, p_factor)
    if (dim(ret)[1L] != length(pval)) stop('demographic table contruction wrong: pval do not match summary stats')
    ret_compare <- as.matrix(pval)
    colnames(ret_compare) <- paste(c('Signif', txt_g1), collapse = '\n')
  } else ret_compare <- NULL
  
  ret <- cbind(ret, ret_compare)
  attr(ret, which = 'data.name') <- data.name
  attr(ret, which = 'compare') <- compare
  class(ret) <- 'sumtab'
  return(ret)

}









demo_get <- function(x, gidx) {
  # `x`: 'double', 'logical' or 'factor' responses to be compared
  # `gidx`: a 'list' of group indices
  xm <- is.matrix(x)
  xs <- lapply(gidx, FUN = \(i) {
    y <- unclass(if (xm) c(x[i, ]) else x[i])
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
  dnm1 <- paste0('\u2e22', dnm[[1L]], '\u2e25')
  dnm2 <- paste0('\u2e22', dnm[[2L]], '\u2e25')
  id <- lower.tri(pv0, diag = TRUE)
  pv <- pv0[id]
  pnm <- outer(dnm1, dnm2, FUN = paste, sep = ' vs. ')[id]
  sprintf(fmt = paste0(symb(pv), '%.3f; %s'), pv, pnm)
}


# rmd.tzh::label_pvalue_sym
# tzh is not ready to publish \pkg{rmd.tzh}...

# @param pairwise \link[base]{integer} scalar, the maximum group number under which pairwise tests,
# \link[stats]{pairwise.t.test} and \link[stats]{pairwise.wilcox.test}, are preferred.  Default value `3L`.
compare_double <- function(xs, CLT = TRUE, robust = TRUE, pairwise = 3L, alternative = c('two.sided', 'less', 'greater'), ...) {
  
  ng <- length(xs)
  if (ng <= 1L) return('1 arm or less')
  
  alternative <- match.arg(alternative)
  
  p_shapiro <- if (!robust) 1 else vapply(xs, FUN = pval_shapiro, CLT = CLT, FUN.VALUE = 0, USE.NAMES = FALSE)
    
  if (ng == 2L) { # ?stats::t.test or ?stats::wilcox.test
    # always `2L` vs. `1L` (column 1 as reference)
    if (any(p_shapiro < .05)) {
      test <- tryCatch(suppressWarnings(wilcox.test(x = xs[[2L]], y = xs[[1L]], exact = FALSE, conf.int = TRUE, alternative = alternative)), error = identity)
      if (inherits(test, what = 'error')) return('')
      est <- test$estimate
      fmt <- paste0('%.3f', symb(test$p.value), '\n', 'Wilcoxon-\nMann-Whitney')
    } else {
      test <- t.test(x = xs[[2L]], y = xs[[1L]], alternative = alternative)
      if (is.na(test$statistic)) return('') # very likely identical(x, y) in ?stats::t.test
      est <- test$estimate[1L] - test$estimate[2L]
      fmt <- paste0('%.3f', symb(test$p.value), '\n', 'Two-Sample', ' t')
    }
    #return(sprintf(fmt = fmt, est, test$conf.int[1L], test$conf.int[2L], test$p.value))
    return(sprintf(fmt = fmt, test$p.value))
  }

  if (!is.numeric(pairwise) || length(pairwise) != 1L || anyNA(pairwise) || pairwise < 2L) stop('illegal `pairwise`')
  # `is.numeric(pairwise)` not `is.integer(pairwise)` to allow Inf
  
  x <- unlist(xs, use.names = FALSE)
  g <- rep(names(xs), times = lengths(xs, use.names = FALSE))
  
  if (any(p_shapiro < .05)) {
    if (ng <= pairwise) {
      suppressWarnings(tmp <- pairwise.wilcox.test(x = x, g = g, p.adjust.method = 'none', alternative = alternative))
      # ?stats::pairwise.wilcox.test only provides p-value, not confidence intervals
      return(paste(c(pText_pairwise.htest(tmp), 'Pairwise Wilcoxon-Mann-Whitney'), collapse = '\n'))
    }
    
    return(tryCatch(expr = {
      p.value <- kruskal.test(x = x, g = g, ...)$p.value
      sprintf(fmt = paste0(symb(p.value), '%.3f\nKruskal-Wallis'), p.value)
    }, error = 'Kruskal-Wallis test\nnot available'))
  }
  
  if (ng <= pairwise) {
    tmp <- pairwise.t.test(x = x, g = g, pool.sd = FALSE, p.adjust.method = 'none', alternative = alternative)
    return(paste(c(pText_pairwise.htest(tmp), 'Pairwise Two-Sample t'), collapse = '\n'))
  }
  
  # ?stats::aov requires formula~data parameterization
  return(tryCatch(expr = {
    p.value <- summary(aov(x ~ g))[[1L]][1L, 'Pr(>F)']
    sprintf(fmt = paste0(symb(p.value), '%.3f\nANOVA'), p.value)
  }, error = 'ANOVA not available'))
}




compare_bool <- function(xs, pairwise = 3L, alternative = c('two.sided', 'less', 'greater'), ...) {
  
  ng <- length(xs)
  if (ng <= 1L) return('1 arm or less')
  
  alternative <- match.arg(alternative)
  
  xs0 <- lapply(xs, FUN = \(x) x[!is.na(x)])
  X <- vapply(xs0, FUN = sum, FUN.VALUE = 0L, USE.NAMES = TRUE)
  N <- lengths(xs0, use.names = TRUE)
  fish <- tryCatch(fisher.test(cbind(X, N-X), alternative = alternative), error = function(e) {
    if (grepl('consider using \'simulate.p.value=TRUE\'$', e$message)) {
      fisher.test(cbind(X, N-X), alternative = alternative, simulate.p.value = TRUE)
    } else stop(e$message)
  })
  fisher_txt <- sprintf(fmt = paste0(symb(fish$p.value), '%.3f\nFisher\'s Exact'), fish$p.value)
  
  if (ng == 2L) {
    if (any(X == 0L, X == N)) return('') # p-value means nothing
    return(tryCatch(expr = {
      #p.value <- binom.test(x = X, n = N, alternative = alternative)$p.value
      #sprintf(fmt = paste0(symb(p.value), '%.3f\nExact Binomial'), p.value) # sometimes looks wrong..
      p.value <- prop.test(x = X, n = N, alternative = alternative)$p.value
      sprintf(fmt = paste0(symb(p.value), '%.3f\n\u03C7\u00B2 (chi-square)'), p.value)
    }, warning = function(w) fisher_txt))
  }
  
  if (ng <= pairwise) {
    tmp <- suppressWarnings(pairwise.prop.test(x = X, n = N, p.adjust.method = 'none', alternative = alternative))
    return(paste(c(pText_pairwise.htest(tmp), '\u03C7\u00B2'), collapse = '\n'))
  }
  
  return(fisher_txt)
  
}




compare_factor <- function(x, g, ...) {
  # will use ?stats::fisher.test or ?stats::chisq.test even if the factor has 2 levels (i.e. essentially binary)
  
  if (is.matrix(x)) g <- rep(g, times = dim(x)[2L]) # as of 2022-03-08, ?base::table will not recycle shorter argument
  tab <- table(x, g, useNA = 'no') # `x` can be either 'factor' or 'character'
  if (anyNA(tab)) stop('should not happen')
  if (any(dim(tab) == 1L)) return('') # no comparison should be given
  
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



# # write to Word file
# ?flextable:::print.flextable(, preview = 'docx')


