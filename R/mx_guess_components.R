
#' Initial Guess of Univariate Mixture Components
#'
#' Heuristic method to guess number of components and their parameters
#' (mean, standard deviation and weight) from observation data.
#'
#' @param x vector of (non-binned) observation data
#' @param bw bandwidth of kernel density, cf. \code{\link{density}}
#' @param minpeak minimum value of the total maximum which is regarded as peak
#' @param mincut minimum relative height of a pit compared to the lower of
#'   the two neighbouring maxima at which these maxima are regarded as separate
#'   peaks (default value is derived from golden section)
#' @param ... further arguments passed to  \code{\link{density}}
#'
#' @return
#'
#' data frame with mean (\code{mean}), standard deviation \code{sd} and
#'   mixing proportions {L}
#'
#' @details The function guesses approximate start values of parameters
#'   that then need subsequent identification, e.g. with a maximum likelihood or
#'   EM (expectation maximization) method.
#'
#' @seealso \code{\link{peakwindow}}
#'
#' @export
#'
#' @examples
#'
#' set.seed(123)
#' x <- c(rnorm(20, 5, 1), rnorm(30, 10, 1), rnorm(50, 20, 2))
#' hist(x, breaks=20)
#'
#' mx_guess_components(x)
#'
mx_guess_components <- function(x, bw=1, minpeak=0, mincut=0.9, ...) {
  ## previous name: estimate_centers
  ## todo: iterate until npeaks <= 3

  dens <- density(x, bw=bw, na.rm=TRUE, ...)

  ## heuristic algorithm from package cardidates
  pw <- peakwindow(dens, minpeak=minpeak, mincut=mincut)
  npeaks <- length(unique(pw$peakid))

  ## make list of data frames
  dens.split <- split(as.data.frame(dens[c("x", "y")]), pw$peakid)

  ## weighted means and standard deviations
  means <- sapply(dens.split, function(x) weighted.mean(x$x, x$y))
  ysum  <- sum(dens$y)

  L <- sds <- numeric(npeaks)
  for (i in 1:length(means)) {
    #cat(i, "\n")
    yi <- dens.split[[i]]$y
    xi <- dens.split[[i]]$x
    # still wrong
    #sds[i] <- weighted.sd(xi, yi, df=length(xi)) # !!! important, see stats::cov.wt
    # alternative:
    sds[i] <- sqrt(cov.wt(data.frame(xi), yi)$cov[1])
    L[i] <- sum(yi)
  }

  data.frame(
    mean=means,
    sd=sds,
    L = L/ysum
  )
}
