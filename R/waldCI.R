setClass("waldCI",
         slots = c(level = "numeric",
                   mean = "numeric",
                   sterr = "numeric"))

setValidity("waldCI", function(object) {
  if (object@level <= 0 | object@level >= 1) {
    stop("level must be in (0, 1)")
  }
  if (object@sterr <= 0) {
    stop("stderr must be positive")
  }
  if (!is.finite(object@sterr)) {
    stop("Infinite CI not supported")
  }
  return(TRUE)
})

##' Create `waldCI` object. Either `lb` and `ub`, or `mean` and `sterr` must be
##' provided.
##' @param level Confidence level
##' @param lb lower bound, optional
##' @param ub upper bound, optional
##' @param mean mean, optional
##' @param sterr standard error, optional
##' @return A `waldCI` object
##' @export
##' @importFrom methods new
##' @importFrom stats qnorm
##' @examples
##'  myci <- makeCI(.95, mean = 2, sterr = 1)
makeCI <- function(level,
                   lb = NULL,
                   ub = NULL,
                   mean = NULL,
                   sterr = NULL) {
  if (!is.null(mean) && !is.null(stderr) &&
      is.null(lb) && is.null(ub)) {
    return(new("waldCI", level = level, mean = mean, sterr = sterr))
  } else if (is.null(mean) && is.null(sterr) &&
             !is.null(lb) && !is.null(ub)) {
    if (lb > ub) {
      stop("lb must be less than ub")
    }
    z <- stats::qnorm((1 + level) / 2)
    mean <- (lb + ub) / 2
    sterr   <- (ub - lb) / (2 * z)
    return(methods::new("waldCI", level = level, mean = mean, sterr = sterr))
  } else {
    stop("Input must be either lb/ub or mean/sterr")
  }
}

##' Internal function to compute bounds from stored mean/sterr
##' @param ci Confidence level
##' @return A vector of length 2 for lower and upper bounds
##' @importFrom stats qnorm
.getBounds <- function(ci) {
  z <- stats::qnorm((1 + ci@level)/2)
  lb <- ci@mean - z*ci@sterr
  ub <- ci@mean + z*ci@sterr
  return(c(lb, ub))
}

##' Show method for waldCI object
##' @param object A waldCI object
##' @return `object`, invisibly
setMethod("show", "waldCI", function(object) {
  bound <- .getBounds(object)
  cat(round(object@level*100))
  cat("% CI: (")
  cat(bound[1])
  cat(", ")
  cat(bound[2])
  cat(")\n")
  return(invisible(object))
})
