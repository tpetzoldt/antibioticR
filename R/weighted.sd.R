#' Weighted Standard Deviation
#'
#' Computes the standard deviation of values in x using weights.
#'
#' @param x numeric vector of data
#' @param w numeric vector of weights
#' @param df degrees of freedom
#'
#' @return scalar value
#'
#'
#' @examples
#'
#' x <- c(1.5, 4.1, 3.7, 8.2)
#' w <- c(4, 5, 4, 1)
#' weighted.sd(x, w)
#'
#'
#' @export
#'
weighted.sd <- function(x, w = rep(1, length(x)), df=sum(w) - 1) {
  if (!is.numeric(x) && !is.complex(x) && !is.logical(x)) {
    warning("argument x is not numeric or logical: returning NA")
    return(NA_real_)
  }
  if (!is.numeric(w) && !is.logical(w)) {
    warning("argument w is not numeric or logical: returning NA")
    return(NA_real_)
  }
  if (length(x) != length(w)) stop("x and must have equal length")

  S <- sum(x * w)
  Q <- sum((x^2) * w)
  N <- sum(w)

  ## thpe-fixme, avoid division by zero
  sd <- ifelse(N > 1, sqrt((Q - S*S / N) / (N-1)), Inf)
}
