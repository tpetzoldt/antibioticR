
#' Compare Fitted Mixture Distribution to Data
#'
#' @param parms paremeter list of a ..... mixture distribution
#' @param breaks upper class limits of the data
#' @param counts frequency of observations
#' @param plot boolean, whether to show a diagnostic plot
#' @param ... additional arguments passed to \code{plot}
#'
## #' @return
#'
#' @export
#'
#' @examples
#'
## was: qq_rsquared
mx_compare <- function(parms, breaks, counts, plot=FALSE, ...) {


  cnt <- counts[counts > 0]
  brk <- (breaks[-1])[counts > 0]

  sim <- punimix(brk, parms, full.out=FALSE)
  obs <- cumsum(cnt)/sum(cnt)
  if (plot) {
    op <- par(no.readonly = TRUE)
    par(mfrow=c(1,2))
    plot(brk, sim, ylim=c(0,1), ...)
    lines(brk, obs)
    qqplot(sim, obs); abline(a=0, b=1)
    par(op)
  }

  list(
    r_cor = cor(sim, obs),
    r2_var = 1 - var(sim - obs) / var(obs),
    EF     = 1 - sum((sim - obs)^2) / sum((obs - mean(obs))^2)
  )
}
