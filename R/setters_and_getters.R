##' Internal function to get a named slot
##' @param object A `waldCI` object
##' @param slotname Which slot? Character.
##' @return A length-1 vector with the requested `slotname`.
.getSlot <- function(object, slotname) {
  if (slotname %in% c("level", "mean", "sterr")) {
    return(slot(object, slotname))
  }
  bounds <- .getBounds(object)
  if (slotname == "lb") {
    return(bounds[1])
  } else if (slotname == "ub") {
    return(bounds[2])
  } else {
    stop("Invalid slotname")
  }
}

##' Getters and Setters for Wald CI objects
##' @param object A `waldCI` object
##' @param x A `waldCI` object
##' @param value New value when setting
##' @param ... ignored
##' @return Getters return scalar; setters return the object
##' @rdname ci_setters_getters
##' @export
setGeneric("level", function(object) {
  standardGeneric("level")
})

##' @rdname ci_setters_getters
##' @export
setMethod("level", "waldCI", function(object) .getSlot(object, "level"))

# mean is already s3 generic, so just use that
##' @rdname ci_setters_getters
##' @export
mean.waldCI <- function(x, ...) .getSlot(x, "mean")

##' @rdname ci_setters_getters
##' @export
setGeneric("sterr", function(object) {
  standardGeneric("sterr")
})

##' @rdname ci_setters_getters
##' @export
setMethod("sterr", "waldCI", function(object) .getSlot(object, "level"))

##' @rdname ci_setters_getters
##' @export
setGeneric("lb", function(object) {
  standardGeneric("lb")
})

##' @rdname ci_setters_getters
##' @export
setMethod("lb", "waldCI", function(object) .getSlot(object, "lb"))

##' @rdname ci_setters_getters
##' @export
setGeneric("ub", function(object) {
  standardGeneric("ub")
})

##' @rdname ci_setters_getters
##' @export
setMethod("ub", "waldCI", function(object) .getSlot(object, "ub"))

##' @rdname ci_setters_getters
##' @export
setGeneric("level<-", function(object, value) {
  standardGeneric("level<-")
})

##' @rdname ci_setters_getters
##' @export
setMethod("level<-", "waldCI", function(object, value) {
  return(makeCI(level = value,
                mean = object@mean,
                sterr = object@sterr))
})

##' @rdname ci_setters_getters
##' @export
setGeneric("mean<-", function(object, value) {
  standardGeneric("mean<-")
})

##' @rdname ci_setters_getters
##' @export
setMethod("mean<-", "waldCI", function(object, value) {
  return(makeCI(level = object@level,
                mean = value,
                sterr = object@sterr))
})

##' @rdname ci_setters_getters
##' @export
setGeneric("sterr<-", function(object, value) {
  standardGeneric("sterr<-")
})

##' @rdname ci_setters_getters
##' @export
setMethod("sterr<-", "waldCI", function(object, value) {
  return(makeCI(level = object@level,
                mean = object@mean,
                sterr = value))
})

##' @rdname ci_setters_getters
##' @export
setGeneric("lb<-", function(object, value) {
  standardGeneric("lb<-")
})

##' @rdname ci_setters_getters
##' @export
setMethod("lb<-", "waldCI", function(object, value) {
  return(makeCI(level = object@level,
                lb = value,
                ub = .getBounds(object)[2]))
})

##' @rdname ci_setters_getters
##' @export
setGeneric("ub<-", function(object, value) {
  standardGeneric("ub<-")
})

##' @rdname ci_setters_getters
##' @export
setMethod("ub<-", "waldCI", function(object, value) {
  return(makeCI(level = object@level,
                lb = .getBounds(object)[2],
                ub = value))
})
