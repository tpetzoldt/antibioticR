#' Summary of an 'abr_ecoffinder' Object
#'
#' Returns parameters and quantiles of the normal distribution fitted to
#'   the wild type subset of the data.
#'
#' @param object abr_ecoffinder object
#' @param ... additional arguments affecting the summary produced.
#'
#' @rdname abr_summary
#' @exportMethod summary
#'
setMethod("summary", "abr_baseclass",
          function(object, ...) {
            summary(object@fit, cov=cov, ...)
          }
)

#' @rdname abr_summary
#' @exportMethod summary
#'
setMethod("summary", "abr_ecoffinder",
          function(object, ...) {
            print(summary(object@models[[object@i_best]]))
            cat("---\n")
            cat("ECOFF quantiles:\n")
            print(ecoff(object))
          }
)

