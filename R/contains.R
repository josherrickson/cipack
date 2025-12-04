##' Check whether a CI overlaps with a constant (`contains`) or another CI
##' (`overlap`)
##' @param ci A `waldCI` object
##' @param value A numeric value
##' @param ci1 A `waldCI` object
##' @param ci2 A `waldCI` object
##' @return Logical.
##' @rdname contains_overlaps
##' @export
contains <- function(ci, value) {
  stopifnot(is.numeric(value))
  stopifnot(length(value) == 1)
  bounds <- .getBounds(ci)
  return(value > bounds[1] & value < bounds[2])
}

##' @rdname contains_overlaps
##' @export
overlap <- function(ci1, ci2) {
  b1 <- .getBounds(ci1)
  b2 <- .getBounds(ci2)
  return(max(b1[1], b2[1]) <= min(b1[2], b2[2]))
}
