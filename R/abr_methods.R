## -------------------------------------------------------------
## basic methods for the abr_ - classes
## -------------------------------------------------------------


#' Accessor Methods of Package \pkg{antibioticR}.
#'
#' Functions to access data and results of fitted antibioticR objects:
#' \code{summary}, \code{obs} \code{coeff}, ...
#'
#' @param object name of a 'antibioticR' object.
## @param cov logical; if TRUE, the covariance matrix of the estimated
#'   parameters is returned and printed.
#' @param \dots other arguments passed to the methods.
#'
#'
#' @rdname abr_methods
#' @exportMethod obs
#'
setGeneric("obs", function(object, ...) standardGeneric("obs"))

#' @rdname mxObj-methods
#'
setGeneric("fit", function(object, ...) standardGeneric("fit"))

#' @rdname mxObj-methods
#'
setGeneric("ecoff", function(object, ...) standardGeneric("ecoff"))

#' @rdname mxObj-methods
#'
setGeneric("startpar", function(object, ...) standardGeneric("startpar"))

#' @rdname mxObj-methods
#'
setGeneric("models", function(object, ...) standardGeneric("models"))

# ------------------------------------------------------------------------------
# abr-baseclass-methods
# ------------------------------------------------------------------------------


#' @rdname abr_methods
#' @exportMethod obs
#'
setMethod("obs", "abr_baseclass",
          function(object, ...) {
            object@data
          }
)

#' @rdname abr_methods
#' @exportMethod coef
#'
setMethod("coef", "abr_baseclass",
          function(object, ...) {
            #coef(object@fit, ...)
            object@coef
          }
)

#' @rdname abr_methods
#' @exportMethod fit
#'
setMethod("fit", "abr_baseclass",
          function(object, ...) {
            object@fit
          }
)


#' @rdname abr_methods
#' @exportMethod deviance
#'
setMethod("deviance", "abr_baseclass",
          function(object, ...) {
            deviance(object@fit, ...)
          }
)


#' @rdname abr_methods
#' @exportMethod residuals
#'
setMethod("residuals", "abr_baseclass",
          function(object, ...) {
            residuals(object@fit, ...)
          }
)

#' @rdname abr_methods
#' @exportMethod df.residual
#'
setMethod("df.residual", "abr_baseclass",
          function(object, ...) {
            df.residual(object@fit, ...)
          }
)

# ------------------------------------------------------------------------------
# abr-ecoffinder-methods
# ------------------------------------------------------------------------------

#' @rdname abr_methods
#' @exportMethod startpar
#'
setMethod("startpar", "abr_ecoffinder",
          function(object, ...) {
            object@startpar
          }
)

#' @rdname abr_methods
#' @exportMethod models
#'
setMethod("models", "abr_ecoffinder",
          function(object, ...) {
            object@models
          }
)

#' @rdname abr_methods
#' @exportMethod ecoff
#'
setMethod("ecoff", "abr_ecoffinder",
          function(object, ...) {
            #if (object@log2)
              #cat("Note: log2 used for the transformation of MIC data!\n")
            object@ecoff
          }
)
