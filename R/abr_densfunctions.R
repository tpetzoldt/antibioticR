

#' Kernel Density Estimate for ZD and MIC data
#'
#' Estimate kernel density for antibiotic resistance data using either standard
#'   density estimation with Gaussian kernels or boundary corrected density estimation
#'   with function \code{pbckden} from package \pkg{evmix}.
#'
#' @param x vector of antibiotic resistance observations, given as zone
#'   diameter (zd) or log of minimum inhibitory concentration (mic).
#'   Note that for \code{abr_density} x must contain raw observations,
#'   not frequencies, cf. \code{\link{unbin}}, while for
#'   abr_cumdens x and y contain binned data. If y is omitted, x must contain a
#'   two-column matrix with value and frequency.
#' @param cutoff cutoff value (disc diameter) in case of zd data
#' @param method character which density method to use: \code{'density'},
#'   \code{'evmix'} (function \code{pbckden}), \code{'spline'},
#'    or \code{'fmm'} (monotone spline)
#' @param control options passed to the density estimation methods, see \code{\link{abr_density.control}}
#' @param bw the smoothing bandwidth to be used,
#'   see \code{\link{density}} for details.
#' @param cut by default, the values of from and to are cut bandwidths beyond
#'   the extremes of the data, see \code{\link{density}} for details
#' @param proper logical, whether density is renormalised to integrate to unity
#' @param bcmethod character, boundary correction method
#' @param n the number of equally spaced points at which the density is to be
#'   estimated, see \code{\link{density}} for details
#' @param p vector of probability quantiles
#' @param from,to search range for maximum (= mode) search
#'
#'
#' @return data frame with
#'   \itemize{
#'     \item x zone diameter resp. mic value
#'     \item y density estimate
#'   }
#'
#' @seealso \code{\link{density}}, \code{\link{dbckden}}
#'
#' @rdname abr_densfunctions
#'
#' @export
#'
#' @examples
#'
#' opar <- par(no.readonly = TRUE)
#' ## ===== minimum inhibitory concentrations (MIC) data =====
#' data("micdata")
#' micdata$freq <- with(micdata, ifelse(is.na(freq), 0, freq))
#'
#' par(las = 2, mar=c(9,4,4,2)+.1)
#'
#' ##' relative frequencies
#' y <- micdata$freq / sum(micdata$freq)
#' x <- log2(micdata$conc)
#'
#' plot(y ~ x, type = "h", axes = FALSE, xlab="", ylab="rel. frequency")
#' axis(2)
#' axis(1)
#' axis(1, line=3, at = log2(micdata$conc), label = round(micdata$conc, 4))
#' mtext("log2(conc)", side=1, line=1.5, las=1)
#' mtext("conc", side=1, line=6, las=1)
#'
#' ##' standard R function with weights
#' dens <- density(x, weights=y, bw=2/3)
#' lines(dens)
#'
#' ## ===== zone diameter (ZD) data =====
#' freq <- c(36, 0, 2, 3, 4, 8, 9, 14, 10, 9, 3, 1, 8, 2, 4, 8, 20,
#'           45, 40, 54, 41, 22, 8, 3, 3, 0, 0, 0, 0, 2)
#'
#' relfreq <- freq/sum(freq)
#' bins <- 5 +(1:length(freq))
#' mids <- bins + 0.5
#'
#' plot(mids, relfreq, type="h")
#'
#' ## ----- standard R function -----
#' edf <- density(mids, weights=relfreq, bw=2/3, from=0, to=40)
#' lines(edf)
#'
#' ## ----- same, but with different interface -----
#' edf2 <- abr_density(unbin(mids, freq), control=list(bw=2/3))
#' lines(edf2, col="red", lty="dotted")
#'
#' ## ----- boundary corrected density from package evmix -----
#' bcd <- abr_density(unbin(mids, freq), method="evmix")
#' lines(bcd, col="blue", lty="dotted")
#'
#' ## ===== maximum of density function (uses optimizer) =====
#' maxdens(unbin(mids, freq))
#'
#' ## ----- cumulative density -----
#' plot(abr_cumdens(unbin(mids, freq), method="fmm"), type="l")
#'
#' ## ----- quantiles -----
#' prob <- c(0.5, 0.95, 0.99)
#' abline(h=prob, lty="dotted")
#' abline(v=abr_cumdensquant(unbin(mids, freq),
#'                           p=prob, method="fmm"), lty="dotted")
#'
#' par(opar)
#'
#'
#'
abr_density <- function(x, cutoff=5.5, method = c("density", "evmix"),
                        control=abr_density.control()) {

  method <- match.arg(method)

  ctrl <- abr_density.control()
  if(!missing(control)) {
    control <- as.list(control)
    ctrl[names(control)] <- control
  }

  if (method == "evmix") {
    ## 3 is cut parameter in 'density' function
    ret <- with(ctrl, {
      xnew <- seq(0, max(x - cutoff) + 3 * bw, length.out=n)
      y <- dbckden(xnew, x - cutoff, bw = bw, bcmethod = bcmethod, proper = proper)
      data.frame(x = xnew + cutoff, y = y)
    })
  } else {
    ret <- with(ctrl, density(x, bw = bw, cut=cut, from = cutoff, n = n))
  }
  ret
}

#' @rdname abr_densfunctions
#' @export
#'
abr_cumdens <- function(x, cutoff=5.5, method = c("evmix", "density", "spline", "fmm"),
                        control=abr_density.control()) {

  method <- match.arg(method)

  ctrl <- abr_density.control()
  if(!missing(control)) {
    control <- as.list(control)
    ctrl[names(control)] <- control
  }

  if (method == "evmix") {
    ## 3 is cut parameter in 'density' function
    ret <- with(ctrl, {
      xnew <- seq(-0.1, max(x - cutoff) + cut * bw, length.out=n)
      y <- pbckden(xnew, x - cutoff, bw = bw, bcmethod = bcmethod, proper = proper)
      data.frame(x = xnew + cutoff, y = y)
    })
  } else if (method == "spline") {
    ## empirical cumulative density with spline
    tb <- table(x)
    x <- as.numeric(names(tb))
    y <- cumsum(as.numeric(tb))
    ret <- data.frame(spline(x, y/max(y), n=ctrl$n))

  } else if (method == "fmm") {
    ## use of monotone spline
    Fn <- ecdf(x)
    Spl <- splinefun(x, Fn(x), method="fmm")
    xnew <- seq(min(x), max(x), length.out=ctrl$n)
    ret <- data.frame(x=xnew, y=Spl(xnew))

  } else { # smoothed, from density estimate with Gaussian Kernel
    ## heutistic approximation! -5*bw = statt from left with -5 * sd
    ret   <- with(ctrl, density(x, bw = bw, cut=cut, from = -5*bw, n = n))
    ret <- data.frame(x=ret$x, y=ret$y) # because density returns a list
    ret$y <- cumsum(ret$y)/sum(ret$y)
    ret <- ret[ret$x >= cutoff, ]
  }
  ret
}

#' @rdname abr_densfunctions
#' @export abr_density.control
#' @usage abr_density.control(bw = 1, cut = 3,  n=512, bcmethod = "renorm", proper = TRUE)
#'
abr_density.control <- function(bw = 1, cut = 3,  n=512, bcmethod = "renorm", proper = TRUE)
  list(bw = bw, cut = cut,  n=n, bcmethod = bcmethod, proper = proper)


## estimate quantile (e.g. median) from density curve
##
## todo: make it a method that works with both rawdata and density estimates
##
#' @rdname abr_densfunctions
#' @export
#'
abr_cumdensquant <- function(x, p=0.5, cutoff=5.5,
                          method = c("density", "evmix", "spline", "fmm"),
                          control=abr_density.control()) {

  method <- match.arg(method)

  ctrl <- abr_density.control()
  if(!missing(control)) {
    control <- as.list(control)
    ctrl[names(control)] <- control
  }

  dens <- with(ctrl, abr_cumdens(x, cutoff=cutoff, method=method, control=control))
  approx(dens$y, dens$x, p)$y
}


## find mode( maximum value of density curve)
#' @rdname abr_densfunctions
#' @export
#'
maxdens <- function(x, cutoff=5.5, from=min(x), to=max(x),
                    method = c("density", "evmix"),
                    control=abr_density.control()) {

  ## helper functions for optimization
  f_dens <- function(mx) {

    my <- with(ctrl, density(x, bw = bw, from=mx, to=mx, cut=cut, n = 1)$y)
    #cat("dens ", mx, my, "\n")
    my
  }

  f_dbck <- function(mx) {

    my <- with(ctrl, dbckden(mx, x - cutoff, bw = bw, bcmethod = bcmethod, proper = proper))
    #cat("dbck ", mx, my, "\n")
    my
  }

  ## parameter checks
  method <- match.arg(method)

  ctrl <- abr_density.control()
  if(!missing(control)) {
    control <- as.list(control)
    ctrl[names(control)] <- control
  }

  ## initial guess
  if (method == "density") {

    f <- f_dens

    dd <- with(ctrl, density(x, bw=bw, from=from, to=to, cut=cut, n=128))
    ii <- (1:length(dd$x))[dd$y == max(dd$y)]
    mx <- dd$x[ii]

  } else {

    f <- f_dbck

    xx <- with(ctrl, seq(0, max(x - cutoff) + 3 * bw, length.out=128))
    y  <- with(ctrl, dbckden(xx, x - cutoff,  bw = bw, bcmethod = bcmethod, proper = proper))

    ## special !!! avoid peaks out of search range,
    ## e.g. left exponential for zd data or right peak for mic data
    y[(xx <= from) | (to <= xx)] <- 0

    ii <- (1:length(xx))[y == max(y)]
    mx <- xx[ii]
  }

  ## post-optimization
  mx <- optimize(f, mx + ctrl$bw * c(-5, +5), maximum=TRUE)$maximum

  cat("final solution\n")
  my <- f(mx)

  if (method == "evmix") mx <- mx + cutoff ## make this better

  list(x=mx, y=my)
}

# todo:
# - examples
# - from, to arguments where still missing
