


#' @title \eqn{p}-value from modified Shapiro-Wilk Normality Test
#' 
#' @description
#' Obtain \eqn{p}-value from \link[stats]{shapiro.test}, 
#' taking into consideration of several exceptions.
#' 
#' @param x \link[base]{double} \link[base]{vector}
#' 
#' @param CLT \link[base]{logical} scalar, whether to allow the use of Central Limit Theorem, default `FALSE`
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @details 
#' 
#' Function [pval_shapiro] provides a pseudo \eqn{p}-value for several exceptions of
#' \link[stats]{shapiro.test}, serving as a criteria of whether robust statistics/tests need to be used
#' \describe{
#' \item{`length(x) < 3L`}{returns \eqn{p=0}, indicating that robust methods are needed.}
#' \item{`length(x) > 5e3L`}{returns \eqn{p=1}, indicating that no robust method is needed.  For such large sample size, robust methods could be too slow.}
#' \item{`CLT & length(x) > 30L`}{returns \eqn{p=1}, indicating that no robust method is needed because of the use of Central Limit Theorem.}
#' \item{all `x` values are \link[base]{identical} (or \link[base]{all.equal}, to be implemented in future release)}{returns \eqn{p=0}, indicating that robust methods are needed.}
#' \item{Otherwise}{use the \eqn{p}-value from \link[stats]{shapiro.test}.}
#' }
#' 
#' @returns 
#' 
#' Function [pval_shapiro] returns a \link[base]{double} scalar.
#' 
#' @examples 
#' pval_shapiro(rnorm(5))
#' sapply(with(airquality, split(Ozone, f = Month)), FUN = pval_shapiro)
#' 
#' @importFrom stats shapiro.test
#' @export
pval_shapiro <- function(x, CLT = FALSE, ...) {
  x0 <- as.double(x[!is.na(x)]) # ?stats::shapiro.test will drop NA though
  n <- length(x0)
  # R 4.0.*, ?stats::shapiro.test do not allow sample size <3L or >5e3L
  if (n < 3L) return(0) # robust methods needed for n<3L
  if (n > 5e3L) return(1) # robust methods might be too slow for n>5e3L
  if (n > 30L && CLT) return(1) # central limit theorem, no need for robust methods
  if (all(duplicated.default(x0)[-1L])) return(0) # robust methods needed for all-equal input
  out <- shapiro.test(x0) # let err (?stats::shapiro.test takes no additional parameter)
  if (is.na(pv <- unname(out$p.value))) stop('stats::shapiro.test gives NA $p.value')
  return(pv)
}





