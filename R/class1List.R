

#' @title First Class of Each Element in a Recursive Object
#' 
#' @description 
#' First \link[base]{class} of each element in a \link[base:is.recursive]{recursive} object
#' 
#' @param x a \link[base:is.recursive]{recursive} object, e.g., \link[base]{data.frame} or \link[base]{list}
#' 
#' @returns 
#' Function [class1List] returns a \link[base]{list} of \link[base]{character} elements.
#' Each element is a collection of the names of the elements of each specific class.
#' 
#' @examples 
#' class1List(esoph)
#' class1List(lm(Ozone ~ Wind + Temp, data = airquality))
#'
#' @export 
class1List <- function(x) {
  if (!is.recursive(x)) stop('input must be recursive (data.frame or list)')
  if (is.data.frame(x)) x <- as.data.frame(x)
  if (anyDuplicated.default(names(x))) stop('do not allow duplicated names in the input')
  
  cl1 <- vapply(x, FUN = function(x) class(x)[1L], FUN.VALUE = '', USE.NAMES = TRUE)
  
  return(split.default(names(cl1), f = factor(cl1)))
}