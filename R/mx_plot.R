#' Plot Components of Mixture Distribution
#'
#' @param obj Object of class \code{\link{mxObj}}.
#' @param breaks class boundaries
#' @param counts frequency of observations
#' @param disc disc diameter (defaults to zero)
#' @param ecoff.prob probability threshold for the ecoff
#' @param main main title of the plot
#' @param ... other arguments passed to \code{\link{hist}}
#'
#' @return ecoff value (normal quantile)
#'
#' @examples
#'
#' breaks <- 0:28
#' counts <- c(36, 0, 2, 3, 4, 8, 9, 14, 10, 9, 3, 1, 1, 2,
#'             4, 8, 20, 45, 40, 54, 41, 22, 8, 3, 3, 0, 0,0)
#'
#' observations <- unbin(breaks[-1], counts) # upper class boundaries
#'
#' (comp <- mx_guess_components(observations, bw=2/3, mincut=0.9))
#'
#' obj <- mxObj(comp, left="e")
#'
#' obj2 <- mx_metafit(breaks, counts, obj)
#'
#' mx_plot(obj2, breaks, counts, disc=5.5)
#'
#' ## simplification for fitted objects with save.data = TRUE
#' mx_plot(obj2, disc=5.5)
#'
#' @export
#'
mx_plot <- function(obj, breaks, counts, disc=0, ecoff.prob=0.01,
                    main="Subpopulations", ...) {

  ## old name: ## plot3mix

  if (is(obj, "mxMle")){
    saved_data <- get_data(obj)

    if (length(saved_data) > 0) {
      breaks <- saved_data$breaks
      counts <- saved_data$counts
    }
  }

  if(is.null(breaks) | is.null(counts)) stop("data missing")


  ## correct if non-zero offset
  breaks <-  breaks + disc

  x <- unbin(breaks[-1], counts)

  ## do the opposite here, u.e. convert obj to obj
  #if(is(obj, "mxObj")) obj <- coef(obj)

  ## !!! 0.1 = fixed start, 45 = fixed end
  x1 <- seq(min(breaks), max(breaks), length.out=200)
  xy <- dunimix(x1 - disc , obj, full.out=TRUE)
  xy[, 1] <- xy[,1] + disc
  colors <- c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69')

  nm <- colnames(xy)


  hist(x, breaks=breaks, probability=TRUE, main=main, ...)

  #1st column is x, last is sum
  matlines(xy[, 1], xy[,-c(1,  ncol(xy))], lty=1, lwd=1, col="grey")

  if ("e1" %in% nm) lines(xy[,1], xy[,"e1"], lwd=2, col=colors[6], lty=1)

  it <- mx_intermediate(obj, as.vector=FALSE)
  wt <- mx_wildtype(obj, as.vector=FALSE)


  if (!is.na(it$type))
    lines(x1, it$L * dnorm(x1 - disc, it$mean, it$sd), lwd=2, col=colors[7], lty=1)

  if (!is.null(wt$type))
    lines(x1, wt$L * dnorm(x1 - disc, wt$mean, wt$sd), lwd=3, col=colors[1], lty=1)

  wt <- mx_wildtype(obj, as.vector = TRUE)
  d <- dnorm(x1, wt["mean"], wt["sd"])
  #lines(x1, wt$L * d)

  ecoff <- qnorm(ecoff.prob, wt["mean"], wt["sd"]) + disc

  abline(v=ecoff, col="black", lty="dashed")
  text(ecoff, 0.7 * par("usr")[4], paste("cutoff =", round(ecoff, 1)), pos=2, srt=90, cex=0.7)

  parms <- coef(obj)

  means <- sapply(parms, function(x) ifelse(is.null(x$mean), 0, x$mean))
  sds   <- sapply(parms, function(x) ifelse(is.null(x$sd), 0, x$sd))
  L     <- sapply(parms, function(x) x$L)

  text(means + disc, 0.8*par("usr")[4], paste("L =", round(L, 3)), pos=4, cex=0.7, srt=90)

  invisible(ecoff)
}
