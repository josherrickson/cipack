#' @include waldCI.R
NULL

##' Convert waldCI to numeric
##' @param x waldCI object
##' @return Length 2 vector of the bounds of `x`.
setGeneric("as.numeric")

##' @export
setMethod("as.numeric", "waldCI", function(x) {
  return(.getBounds(x))
})
