

#' @title Summary Statistics
#' 
#' @description 
#' Provide the summary statistics of an R object
#' 
#' @param x an R object
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' 
#' Function [.sumstat()] returns a \link[base]{character} scalar.
#' 
#' @examples 
#' x = c(rpois(n = 20L, lambda = 2), NA_integer_)
#' .sumstat(x)
#' 
#' # factor 
#' x = state.region
#' x[2L] = NA_integer_
#' .sumstat(x)
#' 
#' # binary
#' .sumstat(c(TRUE, FALSE, TRUE, NA))
#' .sumstat(c(TRUE, FALSE, TRUE))
#' .sumstat(c(FALSE, FALSE, NA))
#' .sumstat(c(FALSE, FALSE, FALSE))
#' .sumstat(c(NA, NA, NA))
#' 
#' @keywords internal
#' @name sumstat
#' @export
.sumstat <- function(x, fmt, ...) UseMethod('.sumstat')

#' @rdname sumstat
#' 
#' @param fmt (optional) \link[base]{character} scalar, only for function [.sumstat.default], see function \link[base]{sprintf}
#' 
#' @details
#' Function [.sumstat.default()] accepts all R objects of \link[base]{typeof} \link[base]{double} and \link[base]{integer}.
#' Statistics of \link[base]{mean}, \link[stats]{sd} and \link[base]{range} are reported.
#' If the normality assumption via \link[stats]{shapiro.test} is not satisfied, then \link[stats]{median} and \link[stats]{mad} are also reported.
#' 
# @importFrom e1071 skewness
#' @importFrom stats setNames
#' @export .sumstat.default
#' @export
.sumstat.default <- function(x, fmt = '%.2f', ...) {
  
  # 'numeric', 'integer', 'difftime', etc
  
  if (!(typeof(x) %in% c('double', 'integer'))) stop('input must have type double or integer')
  x0 <- unclass(x)
  x <- x0[is.finite(x0)] # NA/Inf/-Inf removed
  
  nm <- c('\n\tmean\u00B1sd\n\tmedian; IQR\n\trange')
  
  if (!(n <- length(x))) return(setNames('', nm = nm))
  
  ret <- character()
  
  ret <- c(ret, if (n < length(x0)) paste0('n*=', n) else '') # align with 'varname:' in DemographicTable
  
  mean_ <- mean.default(x)
  sd_ <- sd(x)
  ret <- c(ret, if (is.na(sd_) || (abs(sd_) < .Machine$double.eps)) { # is.na(sd_) when samplesize = 1
    sprintf(fmt = paste(fmt, '(sd = 0)'), mean_)
  } else sprintf(fmt = paste0(fmt, '\u00B1', fmt), mean_, sd_))
  
  q_ <- quantile(x, probs = c(0, .25, .5, .75, 1))
  iqr <- q_[4L] - q_[2L]
  
  prt_median <- if (n == 1L) FALSE else {
    p_shapiro <- tryCatch(shapiro.test(x)$p.value, error = identity)
    !inherits(p_shapiro, what = 'error') && p_shapiro < .05
  }
  ret <- c(ret, if (prt_median) {
    if (abs(iqr) < .Machine$double.eps) {
      sprintf(fmt = paste(fmt, '(IQR = 0)'), q_[3L])
    } else sprintf(fmt = paste0(fmt, '; ', fmt), q_[3L], iqr)
  } else '.')
  
  ret <- c(ret, if ((n == 1L) || (q_[1L] == q_[5L])) '\t' else {
    paste(sprintf(fmt = fmt, c(q_[1L], q_[5L])), collapse = '~') # range
  })
  
  return(setNames(paste(ret, collapse = '\n'), nm = nm))
  
}



#' @rdname sumstat
#' @export .sumstat.factor
#' @export
.sumstat.factor <- function(x, ...) {
  
  if (!length(x)) return('')
  
  ct <- table(c(x), useNA = 'no')
  # ?base::c important, there might be 'matrix'-'factor' for ?DemographicTable
  # `useNA = 'ifany'` is not allowed
  # stop('useNA either \'no\' or \'always\' to guarantee equal names for all \'groups\' in DemographicTable')
  # Dec 2024: no one wants to use `useNA = 'always'`; this parameter removed
  
  if (anyNA(nm0 <- names(ct))) stop('wont happen')
  nm <- paste(c('', nm0), collapse = '\n\t')
  if (!any(xok <- !is.na(x))) return(setNames('', nm = nm))
  
  ret <- character()
  ret <- c(ret, if (!all(xok)) paste0('n*=', sum(xok)) else '')
  ret <- c(ret, ifelse(ct == 0L, yes = '-', no = sprintf(fmt = '%d (%.1f%%)', ct, 1e2*ct/sum(ct))))
  # decimal point of percentage is hard-coded
  return(setNames(paste(ret, collapse = '\n'), nm = nm))
  
}

#' @rdname sumstat
#' @export
.sumstat.ordered <- .sumstat.factor

#' @rdname sumstat
#' @export 
.sumstat.character <- function(x, ...) .sumstat.factor(factor(x), ...)



#' @rdname sumstat
#' @export
.sumstat.logical <- function(x, ...) {
  warning(msg_logical())
  if (!length(x)) return('')
  xok <- !is.na(x)
  if (!any(xok)) return('')
  ct1 <- sum(x[xok])
  ret <- character()
  ret <- c(ret, if (!all(xok)) paste0('n*=', sum(xok)) else NULL) # not `else ''`
  ret <- c(ret, if (ct1 == 0L) '-' else sprintf(fmt = '%d (%.1f%%)', ct1, 1e2*ct1/sum(xok)))
  # decimal point of percentage is hard-coded
  paste(ret, collapse = '\n')
}


