#' Plot Fitted abrECOFFinder Object
#'
#' @param x an object fitted by \code{\link{ecoffinder_nls}}
#' @param cumulative plot cumulative or density function
#' @param fits whether to plot all fits or only the best
#' @param \dots further parameters passed to \code{plot}
#'
#'
#' @details: to do: documentation and examples
#'
#' @export
#'
setMethod("plot", c("abr_ecoffinder"),
          function(x, cumulative = TRUE, fits = c("all", "best"), ...) {
            fits <- match.arg(fits)
            if (cumulative) {
              plot_pecoff(x, fits, ...)
            } else {
              plot_decoff(x, fits, ...)
            }
          }
)


