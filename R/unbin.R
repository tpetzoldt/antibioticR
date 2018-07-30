#' Unbin Data Frame
#'
#' Unbin a data frame, i.e. replicate rows according to frequencies given in
#' a vector. The function is intended to reconstruct raw data from data
#' binned to classes.
#'
#' @param x a vector or data frame with binned data
#' @param freq vector of frequencies
#'
#' @return
#'
#' Vector or data frame with the same stucture as \code{x} with replicated
#'   elements or rows.
#'
#' @export
#'
#' @examples
#'
#' x <- data.frame(a=1:3, b=c("a", "b", "c"), freq=c(2, 5, 3))
#' unbin(x, x$freq)
#'
#' zd <- 6:35
#'
#' counts <- c(155, 0, 8, 12, 17, 35, 37, 66, 42, 39, 13, 4, 4, 8, 19, 36,
#'             80, 205, 188, 219, 170, 104, 32, 13, 12, 0, 3, 0, 0, 0)
#'
#' observations <- unbin(zd, counts)
#'
unbin <- function(x, freq) {
  #if (!is.data.frame(x)) stop("df must be a data.frame")
  if (!is.vector(freq)) stop("freq must be a vector")

  if (is.data.frame(x)) {
    ret <- x[rep(1:nrow(x), freq), ]
    row.names(x) <- NULL
  } else if (is.vector(x)) {
    ret <- rep(x, freq)
  } else {
    stop("x must be a vector or data.frame")
  }
  ret
}

