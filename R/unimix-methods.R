#' Univariate Mixtures of Exponential, Normal and Gamma Distributions
#'
#' Density, distribution function, quantile function and random generation for
#'   univariate mixture distributions, composed of exponential, normal and gamma
#'   distributions.
#'
#' @param x vector of quantiles
#' @param q vector of quantiles
#' @param obj obkect of class \code{mxObj} with mixture distribution parameters
#' @param n number of random values
#' @param full.out return total density only or a matrix with all components
#' @param ... reserved for future extensions
#'
#' @return dunimix gives the density, punimix gives the distribution function,
#'   qunimix gives the quantile function, and runimix generates random deviates.
#'   The length of the result is determined by the legnt of x.
#'  Argument \code{full.out} determines whether only a vector with the totals of
#'    the distribution or a data frame with all components is returned.
#'
#'
#' @note A quantile function \code{qunimix} is not yet implemented.
#'   Automatic coercion of arguments may still have bugs.
#'
#'
#' @examples
#'
#' @rdname unimix
#'
setGeneric("dunimix", function(x, obj, full.out, ...) dunimix(x, obj, full.out, ...))

#' @rdname unimix
#'
setGeneric("punimix", function(q, obj, full.out, ...) punimix(q, obj, full.out, ...))
# setGeneric("qunimix", function(q, object, ...) qunimix(q, obj, ...))

#' @rdname unimix
#'
setGeneric("runimix", function(n, obj, ...)  runimix(n, obj, ...))


#' @rdname unimix
#' @exportMethod dunimix
setMethod("dunimix", c("numeric", "list"),
          function(x, obj, ...) {
            .dunimix(x, obj, ...)
          }
)


#' @rdname unimix
#' @exportMethod dunimix
setMethod("dunimix", c("numeric", "mxObj"),
          function(x, obj, full.out=FALSE, ...) {
            .dunimix(x, coef(obj), full.out=full.out, ...)
          }
)


#' @rdname unimix
#' @exportMethod punimix
setMethod("punimix", c("numeric", "list"),
          function(q, obj, full.out=FALSE, ...) {
            .punimix(q, obj, full.out, ...)
          }
)


#' @rdname unimix
#' @exportMethod punimix
setMethod("punimix", c("numeric", "mxObj"),
          function(q, obj, ...) {
            .punimix(q, coef(obj), ...)
          }
)


## not yet implemented
#'
#' #' @rdname unimix
#' #' @exportMethod qunimix
#' #'
#' setMethod("qunimix", c("numeric", "mxObj"),
#'           function(x, obj, ...) {
#'             qunimix(x, coef(obj), ...)
#'           }
#' )



#' @rdname unimix
#' @exportMethod runimix
setMethod("runimix", c("numeric", "list"),
          function(n, obj, ...) {
            .runimix(n, obj, ...)
          }
)

#' @rdname unimix
#' @exportMethod runimix
setMethod("runimix", c("numeric", "mxObj"),
          function(n, obj, ...) {
            .runimix(n, coef(obj), ...)
          }
)
