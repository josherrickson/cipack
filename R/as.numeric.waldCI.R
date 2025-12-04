##' Convert waldCI to numeric
##' @param x waldCI object
##' @return Length 2 vector of the bounds of `x`.
##' @export
setGeneric("as.numeric")

setMethod("as.numeric", "waldCI", function(x) {
  return(.getBounds(x))
})
