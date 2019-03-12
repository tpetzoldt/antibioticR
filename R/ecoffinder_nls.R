#' Find Cutoff Value by Nonlinear Regression to Truncated Data
#'
#' Identifies the wild-type sub-population by fitting a cumulative normal
#' distribution to subsets of MIC or ID data.
#'
#' @param conc concentration of the antibiotic
#' @param count raw frequency
#' @param startpar start parameters for the nonlinear search,
#'   or "mode" resp. "mean" for an automatic determination
#' @param concentrations which concentrations are tested
#' @param log2 logical determining if conc are log-transformed or not
#' @param plot logical, switch visualization on or off
#'
#' @return an object of class \code{\link{abr_ecoffinder-class}} containing the fitted
#'   parameters and statistics of the final and intermediate fits.
#'
#' @details Start values for the nonlinear regression can be automatically
#'   determined with function \code{\link{ecoffinder_startpar}}.
#'
#'   The default search interval starts one concentration level above the mode resp. mean.
#'
#' @references
#'
#' Turnidge, J., Kahlmeter, G., Kronvall, G. (2006) Statistical characterization of
#'   bacterial wild-type MIC value distributions and the determination of
#'   epidemiological cut-off values. Clin Microbial Infect 12: 418-425
#'   \doi{10.1111/j.1469-0691.2006.01377.x}
#'
#' @seealso
#'   \code{\link{ecoffinder_startpar}} for heuristic method to gues start parameters\cr
#'   \code{\link{ECOFFinder}} for an interactice shiny app
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
#' pstart <- ecoffinder_startpar(x, y)
#' pstart
#'
#' ## nonlinear regression
#' p <- ecoffinder_nls(x, y, pstart)
#' summary(p)
#'
#' @export
#'
ecoffinder_nls <- function(conc, count, startpar="mode", concentrations=NA, log2 = TRUE, plot = TRUE) {

  ## treat NA values:
  ##   - NAs occur at not measured concentrations
  ##   - we assume that they are probably zero
  ##   - Note: be careful when including non-measured data in SSQ computations

  empty <- is.na(count)
  count[empty] <- 0

  ## create local data frame
  data <- data.frame(conc=conc, count=count, cumCount = cumsum(count), notempty = !empty)

  if (startpar[1] %in% c("mode", "mean")) {
    pstart <- ecoffinder_startpar(conc, count) # todo: bandwidth !!!
  } else {
    pstart <- startpar
  }

  ## heuristics to select a suitable concentration range
  if (is.na(concentrations)[1]) {
    cc <- data$conc[data$notempty]
    concentrations <- cc[(cc[1] <= cc) & (pstart["mean"] < cc) & (cc <= cc[length(cc)])]
    ## start one concentration level above mode / mean
    if(length(concentrations) > 1) concentrations <- concentrations[-1]
  }
  cat("Search concentration: ", concentrations, "\n")


  ## plotting
  if (plot) {
    x <- seq(min(conc), max(conc), length.out=100)

    with(data, plot(conc, cumCount))
    with(as.list(startpar), lines(x, pnorm(x, mean=pstart["mean"], sd=pstart["sd"]) * pstart["K"], col="blue"))
  }

  ## fit full data set
  ## todo: handle warning, because of warnOnly=TRUE
  m <- nls(cumCount ~ fnorm(conc, mean, sd, K), data=data, start=pstart,
           control=nls.control(maxiter=1000, warnOnly=TRUE))

  if (plot) lines(x, predict(m, newdata=list(conc=x)), col="green")

  #subsets <- 0:5 # max=last log level available in data
  isubsets <- 1:length(concentrations)

  N_best <- Inf

  models <- vector("list", length(concentrations))

  for (i in isubsets) {
    subset <- data[conc <= concentrations[i], ]

    N <- max(subset$cumCount)

    ## idea: estimate pstart locally for subsets
    # pstart <- c(mean=mean, sd=sd, K=K)

    m <- nls(cumCount ~ fnorm(conc, mean, sd, K), data=subset, start=pstart,
             control=nls.control(maxiter=1000, warnOnly=TRUE))

    models[[i]] <- m

    if (plot) lines(x, predict(m, newdata=list(conc=x)), col="gray", lty="dotted")
    # cat(i, ": ", coef(m), "--", N, "-->")

    N_est <- abs(coef(m)["K"] - N)
    # cat(N_est, "\n")
    if (N_est < N_best) {
      N_best <- N_est
      i_best <- i
      m_best <- m
    }
  }

  pp <- coef(m_best)
  prob <- c(0.95, 0.975, 0.99, 0.999)
  quant <- qnorm(prob, mean=pp["mean"], sd=pp["sd"])

  if (log2 == TRUE) {
    ecoff <- 2^(floor(quant)+1)
  } else {
    ecoff <- quant
  }

  names(ecoff) <- paste0("Q_", prob)
  pp <- coef(m_best)

  if (plot) {
    lines(x, predict(m_best, newdata=list(conc=x)), col="red", lwd=2)
    with(data, points(conc, cumCount, pch=c(1, 16)[1+(data$conc <= concentrations[i_best])], cex=1.5))
    if (log2) {
      abline(v=log2(ecoff), col="red", lty="dashed")
    } else {
      abline(v=ecoff, col="red", lty="dashed")
    }

    axis(1, at=seq(-10, 10, 1), labels=FALSE, tick=TRUE, tcl=-0.25)
  }

  # res <- list(data = data,
  #      concentrations = concentrations, # concentration subsets
  #      startpar = pstart,
  #      i_best = i_best,
  #      fit = m_best,
  #      models = models,
  #      coeff = pp,
  #      quantiles = quant,
  #      ecoff = ecoff,
  #      log2 = log2     # whether ecoffs are 2^ecoff transformed or not
  # )
  # class(res) <- c("list", "abrECOFFinder")
  # res

  res <- new("abr_ecoffinder", data = data,
              concentrations = concentrations, # concentration subsets
              startpar = pstart,
              i_best = i_best,
              fit = m_best,
              models = models,
              coef = pp,
              quantiles = quant,
              ecoff = ecoff,
              log2 = log2     # whether ecoffs are 2^ecoff transformed or not
  )
}
