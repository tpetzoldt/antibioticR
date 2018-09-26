#' Start Values for ECOFFinder
#'
#' A heuristic function functon to estimate start values for the ECOFFinder.
#'
#' @param x x-values (log2 MIC or inhibition zone diameter)
#' @param y y-values (frequencies)
#' @param method select heuristic "peak1", "mode" or "mean"
#' @param bw bandwith of kernel density smoother (see \code{\link{density}})
#'
#' @return vector with estimates for location, scale and total frequency
#'
#'
#' @details Start values for the nonlinear regression are estimated
#'   from the mode and median absolute deviation, from mean and standard
#'   deviation of the data or from a heuristic peak detection method,
#'   borrowed from package \pkg{cardidates}.
#'
#'
#' @references
#'
#' Rolinski, S., Sachse, S. and Petzoldt, T. (2007) cardidates: Identification
#'   of Cardinal Dates in Ecological Time Series. R package version 0.4.8.
#'   \url{https://cran.r-project.org/package=cardidates}
#'
#' Turnidge, J., Kahlmeter, G., Kronvall, G. (2006) Statistical characterization of
#'   bacterial wild-type MIC value distributions and the determination of
#'   epidemiological cut-off values. Clin Microbial Infect 12: 418-425
#'   \doi{10.1111/j.1469-0691.2006.01377.x}
#'
#' @examples
#'
#' ## raw data contain NA values
#' data(micdata)
#' plot(freq ~ log2(conc), data=micdata, type="h")
#'
#' ## discard NA values
#' measured <- na.omit(micdata)
#'
#' ## cumulative plot
#' plot(cumsum(freq) ~ log2(conc), data=measured, type="l")
#'
#' x <- log2(measured$conc)
#' y <- measured$freq
#'
#' ## heuristic start values
#' (pstart <- ecoffinder_startpar(x, y, method = "mean"))
#' (pstart <- ecoffinder_startpar(x, y, method = "mode"))
#' (pstart <- ecoffinder_startpar(x, y, method = "peak1"))
#'
#' @export
#'
ecoffinder_startpar <- function(x, y, method = c("peak1", "mode", "mean"), bw = "nrd0") {
  # todo: check x, y, bw
  method <- match.arg(method)
  if (method == "peak1") {
    y <- ifelse(is.na(y), 0, y)
    unbin <- rep(x, y)
    ret <- mx_guess_components(unbin, bw=bw)
    ## accept component with highest integral as wild type
    ret <- ret[ret$L == max(ret$L), ]
    ret <- unlist(ret[1, c("mean", "sd")])
    K <- sum(y[x < (ret["mean"] + 2*ret["sd"])])   # include all events up to 2sd
    ret <- c(ret, K = K)
  } else if (method == "mode") {
    dx <- diff(x)
    dx <- c(dx, max(dx)) # set heuristic value for last class
    unbin <- rep(x-dx, y)
    dens <- density(unbin, bw=bw)
    #hist(unbin, probability=TRUE)
    #lines(dens)
    mode <- with(dens, x[y==max(y)])
    #abline(v=mode, col="red")
    sd <- mad(unbin, constant=1.43826) # default constant, so that mad approx. sd
    ret <- c(mean=mode, sd=sd, K=sum(y))
  } else {
    ret <- c(
      mean = weighted.mean(x, y),
      #sd = weighted.sd(x, y),
      sd = sqrt(cov.wt(data.frame(x), y)$cov[1]),
      K = sum(y)
    )
  }
  return(ret)
}
