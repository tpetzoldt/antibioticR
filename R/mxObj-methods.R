#' Methods for 'mxObj' Univariate Mixture Objects
#'
#' Constructor and accessor functions of
#'   univariate mixture distributions, composed of exponential, normal and gamma
#'   distributions.
#'
#' @param obj an object of class \code{'mxObj'}
#' @param left whether leftmost distribution is exponential ("e") or normal ("n")
#' @param type type of the mixture distribution, e.g. "n", "en", "nnn", etc.
#' @param x object (vector, list, data.frame) containing mixture distributions
#'   in one of sereral known formats. (thpe-todo: document known formats)
#' @param i index of an individual distribution component
#' @param k penalty parameter (nearly always left at its default value of 2)
#' @param ... optional arguments passed to the methods
#'
#' @rdname mxObj-methods
#'
#'
#' @exportMethod coef
#'
setMethod("coef", "mxObj",
          function(object, i = NULL, ...) {
            #coef(object@fit, ...)
            if (is.null(i)) object@coef else object@coef[[i]]
          }
)

#' @rdname mxObj-methods
#'
setGeneric("pstart", function(obj, ...) standardGeneric("pstart"))

#' @rdname mxObj-methods
#' @exportMethod pstart
#'
setMethod("pstart", "mxObj",
          function(obj, ...) {
            ## plain vector with numbered parameters
            pvec <- as.vector(obj)

            ## same as list
            plist <- as.list(pvec)

            ## remove last mixture weight (L=lambda)
            ## a possible alternative would be smallest weight
            nm <- names(plist)
            wt <- sort(nm[grep("L.*", nm)])
            plist[wt[length(wt)]] <- NULL
            plist
          }
)

## --- mxMle-class -------------------------------------------------------------

#' Methods for 'mxMle' Fitted Univariate Mixture Objects
#'
#' The methods leverage methods from class 'mle2' of package 'bbmle'
#'
#' @param object an object of class \code{'mxObj'}
#'
#' @seealso \code{\link{mle2-class}}
#'
#' @rdname mxObj-methods
#'
#' @exportMethod summary
#'
setMethod("summary", "mxMle",
          function(object, ...) {
            summary(object@fit, ...)
          }
)

#' @rdname mxObj-methods
#'
#' @exportMethod vcov
#'
setMethod("vcov", "mxMle",
          function(object, ...) {
            bbmle::vcov(object@fit, ...)
          }
)


#' @rdname mxObj-methods
#'
#' @exportMethod AIC
#'
setMethod("AIC", "mxMle",
          function(object, ..., k=2) {
            bbmle::AIC(object@fit, ..., k=k)
          }
)

#' @rdname mxObj-methods
#'
#' @exportMethod logLik
#'
setMethod("logLik", "mxMle",
          function(object, ...) {
            bbmle::logLik(object@fit, ...)
          }
)

#' @rdname mxObj-methods
#'
setGeneric("get_data", function(object, ...) standardGeneric("get_data"))

#' @rdname mxObj-methods
#'
#' @exportMethod get_data
#'
setMethod("get_data", "mxMle",
          function(object, ...) {
            object@data
          }
)


# #' @rdname mxObj-methods
# #'
# #' @exportMethod show
# #'
# setMethod("show", "mxMle",
#           function(object) {
#             bbmle::show(object@fit) # original has no dots
#           }
# )

# ... add other methods when needed

## --- constructors ------------------------------------------------------------

setMethod("initialize", signature(.Object="mxObj"),
  function(.Object, ...) {
    .Object <- callNextMethod()
    p <- .Object@coef
    names(.Object@coef) <- paste0(sapply(p, function(x) x$type), 1:length(p))
    invisible(.Object)
  }
)

## encapsulate different uses in same interface
#' @rdname mxObj-methods
#'
#' @export
#'
mxObj <- function(x, type=NA, left=c("e", "n")) {

  ## special method for data frame,
  ## and numeric vector
  ## default method for all others

  if (is.data.frame(x)) {                           ## data frame type
    if (is.null(x$type)) {
      left <- match.arg(left)
      type <- c(left, rep("n", nrow(x)-1))
      obj <- as.mxObj(cbind(type, x))
    } else {
      obj <- as.mxObj(x)
    }
  } else if (is.numeric(x)) {                       ## named vector
    obj <- mxObj(as.list(x))                        ##  -> recursive call
  } else {                                          ## named flat list
    obj <- as.mxObj(x)
  }
  invisible(obj)
}
