##' Apply a monotonic transformation to a `waldCI`
##'
##' Monotonic functions are *not* enforced, but non-monotonic functions that
##' return nonsense results (e.g. ub < lb) will likely error.
##' @param ci A `waldCI` object
##' @param fn Any function that takes in a scalar and returns a scalar.
##' @return The transformed waldCI
##' @export
transformCI <- function(ci, fn) {
  stopifnot(is.function(fn))
  stopifnot(is(ci, "waldCI"))
  return(makeCI(level = ci@level,
                mean = fn(ci@mean),
                sterr = fn(ci@sterr)))
}
