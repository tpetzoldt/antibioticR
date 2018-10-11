#' Fit Mixture Distribution to Data and Evaluate Results
#'
#' @param breaks class boundaries of the data
#' @param counts frequency of observations
#' @param save.data if data should be included in the returned object
#' @param parms list of initial parameters for the mixture, or a suitable
#'   \code{\link{mxObj}}ect with start parameters
#' @param sd_min lower boundary value for standard deviation and rate parameter
#' @param ... additional arguments passed to \code{\link{mle2}} and \code{\link{optim}}
#'
## #' @return
#'
#' @export
#'
#' @examples
#' ## === preparation of data ===
#'
#' zd <- 6:35
#' cutoff <- 6.0
#'
#' counts <- c(155, 0, 8, 12, 17, 35, 37, 66, 42, 39, 13, 4, 4, 8, 19, 36,
#'   80, 205, 188, 219, 170, 104, 32, 13, 12, 0, 3, 0, 0, 0)
#'
#' breaks <- c(zd, (max(zd+1))) - cutoff
#'
#' observations <- unbin(zd, counts) - cutoff
#'
#' ## === quick example ===
#'
#' (comp <- mx_guess_components(observations, bw=2/3, mincut=0.9))
#' obj <- mxObj(comp, left="e")
#'
#' ret <- mx_metafit(breaks, counts, obj)
#' summary(ret)
#'
#' ## === details ===
#'
#' comp <- mx_guess_components(observations, bw=2/3, mincut=0.9)
#' comp # parameters of components in form of a data frame
#'
#' obj <- mxObj(comp, left="n") ## all components normal
#' obj <- mxObj(comp, left="e") ## left component exponential (= default)
#'
#' ## base function, needs parms in correct format
#' ret <-fit_unimix(breaks, counts, parms=pstart(obj), type="enn")
#'
#' ## for more difficult cases, slower
#' ret <-fit_unimix(breaks, counts, parms=pstart(obj), type="enn",
#'         method="BFGS",
#'         control=list(maxit=1000, ndeps=rep(1e-4, length(pstart(obj)))))
#'
#' ## higher level function, determines type from start parameters
#' ret <- mx_metafit(breaks, counts, obj)
#'
#' ## --- evaluation of results --
#' coef(ret)         # top-level mxObj-object, contains all weights
#' coef(ret@fit)     # included mle2-object, last weight missing (sums up to 1.0)
#'
#' summary(ret)      # from bbe::mle
#'
#' AIC(ret)
#' logLik(ret)
#' vcov(ret)          # covariance matrix
#'
#' cov2cor(vcov(ret)) # correlation matrix
#'
mx_metafit <- function(breaks, counts, parms, sd_min=0, save.data=TRUE, ...) {

  validpar <- function(p) {
    nm <- names(p)[!is.na(p)]
    p <- p[!is.na(p)]
    names(p) <- nm
    p
  }

  ## retrieve start parameters from mxObj object
  if (is(parms, "mxObj")) {
    parms <- pstart(parms) # calls S4 method
  }

  vpstart <- validpar(parms)
  npstart <- length(vpstart)

  ## determine mixture type, e.g. "enn"
  nm <- names(unlist(vpstart))
  type <- paste0(c(rep("e", length(grep("rate", nm))),
                      rep("n", length(grep("mean", nm)))), collapse="")

  try(
    if (type == "n") {
      # calculate start values directly, ignore given values
      mids <- 0.5*(breaks[-1] + breaks[-length(breaks)])
      vpstart <- list(mean1 = weighted.mean(mids, counts),
                      #sd1   = weighted.sd(mids, counts)
                      sd1    = sqrt(cov.wt(data.frame(mids), counts)$cov[1])
                      )
      fit <- fit_n(breaks, counts, parms=vpstart, sd_min=sd_min)
    } else {
      fit <-fit_unimix(breaks, counts, vpstart, type, sd_min=sd_min, ...)
    }
  )


  if (!is.null(fit)) {                        ## fit successful
    mxObj <- mxObj(coef(fit))

  } else {                                    ## fit failed
    mxObj <- NULL
  }

  obj <- new("mxMle", mxObj)
  obj@fit <- fit
  if (save.data) obj@data <- list(breaks=breaks, counts=counts)

  obj
}
