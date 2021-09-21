##
#' Extract Main Components from Zone Diameter (ZD) Mixture Distribution
#'
#' Extract np-1 main components and an additional first potentially
#' exponentially distributed component from a data frame of distribution
#' parameters with columns mean, sd, L.
#'
#' @param p data frame of distribution parameters (mean, sd, L) returned by
#'   \code{\link{mx_guess_components}}
#' @param np number of desired components
#' @param near_zero heuristic upper boundary of the zone diameter interval that is
#'   considered as close to zero. Components with mean value below will be considered
#'   as resistant subpopulation.
#'
#' @details The algorithm is specifically intended to zone diameter (ZD)
#'   distributions where the leftmost (resistant) subpopulation is considered
#'   exponentially distributed, while the others (intermediate, wild-type)
#'   are considered approximately normal.
#'
#' @return
#'
#'
#' @export
#'
#' @examples
#'
#'
#' set.seed(123)
#' x <- c(rexp(20, rate=1), rnorm(30, 10, 1), rnorm(10,15,.2), rnorm(50, 20, 2))
#' hist(x, breaks=20)
#' p <- mx_guess_components(x)
#'
#' (ret <- zd_main_components(p, np=3))
#'
#'
zd_main_components <- function(p, np=3, near_zero = 3) {
  p$type ="n"
  ## assume first is exponential
  if (p$mean[1] < near_zero) {
    p[1, "type"] <- "e"
    has.e <- 1
  } else {
    has.e <- 0
  }

  ##         <= (np-1) + has.e
  if (nrow(p) < (np + has.e)) {
    ret <- p
  } else if (p$type[1] == "e") {
    ## the two best, except the first (=resistant)
    ibest <- which((rank(-p$L[2:nrow(p)])) %in% 1:(np-1))

    ## first peak + the one or two other best peaks
    ret <- p[1 + c(0, ibest), ]
  } else {
    ibest <- which(rank(-p$L) %in% 1:(np-1))
    ret <- p[ibest, ]
  }
  ret
}
