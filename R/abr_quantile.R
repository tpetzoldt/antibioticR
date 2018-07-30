
#' Normal Quantiles of Wild Type Population Identified with Ecoffinder
#'
#' Applies \code{\link{qnorm}} to an object returned by \code{ecoffinder_nls}.
#'
#' @param obj object of class \linkS4class{abr_ecoffinder}
#' @param q vector of quantiles
#' @param ... other arguments passed to \code{\link{qnorm}}
#'
#' @details If log2 was used for MIC data,
#'   the results are back-transformed by 2^x.
#'
#' @return
#'
#'   vector of quantiles
#'
#' @export
#'
#' @examples
#'
#'
abr_quantile <- function(obj, q, ...) {
  if (is(obj, "abr_ecoffinder")) {
    log2_used <- obj@log2
    p <- coef(obj)
    ret <- qnorm(q, mean=p["mean"], sd=p["sd"])
  } else {
    cat("not yet implemented")
    ret <- NA
  }
  attributes(ret) <- list(log2_used = log2_used)
  ret
}
