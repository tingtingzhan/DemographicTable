

#' @title Summary Text
#' 
#' @description Provide the summary text of an R object
#' 
#' @param x an R object
#' 
#' @param fmt \link[base]{character} scalar, format string, see \link[base]{sprintf}
#' 
#' @param useNA \link[base]{character} scalar, `'no'` (default) or `'always'`,
#' see \link[base]{table}
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' 
#' Function [summaryText] returns a \link[base]{character} scalar.
#' 
#' @examples 
#' x = rpois(n = 20L, lambda = 2)
#' x[sample.int(length(x), 3L)] = NA_integer_
#' summaryText(x)
#' 
#' # factor 
#' x = state.region
#' x[2L] = NA_integer_
#' summaryText(x)
#' 
#' # binary
#' summaryText(c(TRUE, FALSE, TRUE, NA))
#' summaryText(c(TRUE, FALSE, TRUE))
#' summaryText(c(FALSE, FALSE, NA))
#' summaryText(c(FALSE, FALSE, FALSE))
#' summaryText(c(NA, NA, NA))
#' 
#' @name summaryText
#' @export
summaryText <- function(x, fmt, ...) UseMethod('summaryText')

#' @rdname summaryText
#' @importFrom e1071 skewness
#' @export
summaryText.default <- function(x, fmt = '%.2f', ...) {
  
  # 'numeric', 'integer', 'difftime', etc
  
  if (!(typeof(x) %in% c('double', 'integer'))) stop('input must have type double or integer')
  x_orig <- unclass(x)
  x <- x_orig[is.finite(x_orig)] # NA/Inf/-Inf removed
  
  #nm <- c('\n\tmean\u00B1sd\n\tmedian\u00B1IQR\n\tgeometric mean\n\tSkewness; Shapiro-Wilk\n\tRange')
  #nm <- c('\n\tmean\u00B1sd\n\tmedian\u00B1IQR\n\tSkewness; Shapiro-Wilk\n\tRange')
  nm <- c('\n\tmean\u00B1sd\n\tmedian; IQR\n\tSkewness; Shapiro-Wilk\n\tRange')
  
  if (!(n <- length(x))) {
    ret <- ''
    names(ret) <- nm
    return(ret)
  }
  
  qs <- quantile(x, probs = c(.25, .5, .75))
  
  out <- paste(
    
    if (n < length(x_orig)) paste0('N*=', n) else '', # align with 'varname:' in DemographicTable
    
    {
      sd_ <- sqrt(var(x, na.rm = FALSE))
      if (is.na(sd_) || (abs(sd_) < .Machine$double.eps)) { # is.na(sd_) when samplesize = 1
        sprintf(fmt = paste(fmt, '(sd = 0)'), mean.default(x))
      } else sprintf(fmt = paste0(fmt, '\u00B1', fmt), mean.default(x), sd_)
    },
    
    # paste(sprintf(fmt = fmt, qs), collapse = '\u275a'), # quantiles; obsolete
    
    {
      iqr <- qs[3L] - qs[1L]
      # mad(x) # median +/- MAD; obsolete
      if (abs(iqr) < .Machine$double.eps) {
        sprintf(fmt = paste(fmt, '(IQR = 0)'), qs[2L])
      } else sprintf(fmt = paste0(fmt, '; ', fmt), qs[2L], iqr)
    },

    #  if (any(x <= 0)) '' else { # geometric mean (geometric sd too difficult for the clinicians)
    #    logx <- log(x)
    #    # EnvStats::geoSD(c(1, 1, 1, 1)) # = 1; I don't think it make sense..
    #    #geosd <- if (is.na(.geosd <- sd(logx))) NA_real_ else if (.geosd == 0) 0 else exp(.geosd)
    #    sprintf(exp(mean.default(logx)), fmt = fmt)
    #  },
    
    if (n == 1L) '\t' else {
      x_skew <- skewness(x)
      p_shapiro <- tryCatch(shapiro.test(x)$p.value, error = identity)
      if (is.na(x_skew)) '\t' else {
        if (inherits(p_shapiro, what = 'error')) {
          sprintf(fmt = '%.1f', x_skew) 
        } else if (p_shapiro < .05) {
          sprintf(fmt = '%.1f; \u26a0', x_skew)
          #sprintf(fmt = '%.1f; \u26a0 %.3f', x_skew, p_shapiro)
        } else sprintf(fmt = '%.1f \u2713', x_skew) #sprintf(fmt = '%.1f; %.3f', x_skew, p_shapiro)
      }
    },
    
    if (n == 1L) '\t' else {
      minx <- min(x)
      maxx <- max(x)
      if (minx == maxx) '\t' else paste(sprintf(fmt = fmt, c(minx, maxx)), collapse = '~') # range
    },
    
    sep = '\n')

  names(out) <- nm
  return(out)
}



#' @rdname summaryText
#' @export
summaryText.factor <- function(x, fmt = '%.1f', useNA = c('no', 'always'), ...) {
  useNA <- match.arg(useNA)
  # 'ifany' is not allowed
  # stop('useNA either \'no\' or \'always\' to guarantee equal names for all \'groups\' in DemographicTable')
  if (!length(x)) return('')
  ct <- table(c(x), useNA = useNA, ...)
  # ?base::c important, there might be 'matrix'-'factor' for ?DemographicTable
  if (any(id <- is.na(names(ct)))) names(ct)[id] <- '<NA>'
  out0 <- ifelse(ct == 0L, yes = '-', no = sprintf(fmt = paste0('%d (', fmt, '%%)'), ct, 1e2*ct/sum(ct)))
  nm <- paste(c('', names(ct)), collapse = '\n\t')
  if (!any(xok <- !is.na(x))) {
    ret <- ''
    names(ret) <- nm 
    return(ret)
  }
  ret <- paste(c(if (!all(xok)) paste0('N*=', sum(xok)) else '', out0), collapse = '\n')
  names(ret) <- nm 
  return(ret)
}

#' @rdname summaryText
#' @export
summaryText.ordered <- summaryText.factor

#' @rdname summaryText
#' @export 
summaryText.character <- function(x, ...) summaryText.factor(factor(x), ...)



#' @rdname summaryText
#' @export
summaryText.logical <- function(x, fmt = '%.1f', ...) {
  xok <- !is.na(x)
  if (!any(xok)) return('')
  ct1 <- sum(x[xok])
  out0 <- if (ct1 == 0L) '-' else sprintf(fmt = paste0('%d (', fmt, '%%)'), ct1, 1e2*ct1/sum(xok))
  paste(c(if (!all(xok)) paste0('N*=', sum(xok)), out0), collapse = '\n')
}


