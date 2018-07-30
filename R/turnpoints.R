#' Find turning points (peaks or pits)
#' Determine the number and the position of extrema (turning points, either peaks or pits) in a regular time series.
#'
#' @param x vector or a time series
#'
#' @return
#'
#' An object of type 'abrTurnpoints' is returned, with:
#' \item{data}{ The dataset to which the calculation is done }
#' \item{n}{ The number of observations }
#' \item{points}{ The value of the points in the series, after elimination of ex-aequos }
#' \item{pos}{ The position of the points on the time scale in the series (including ex-aequos) }
#' \item{exaequos}{ Location of exaequos (1), or not (0) }
#' \item{nturns}{ Total number of tunring points in the whole time series }
#' \item{firstispeak}{ Is the first turning point a peak (\code{TRUE}), or not (\code{FALSE}) }
#' \item{peaks}{ Logical vector. Location of the peaks in the time series without ex-aequos }
#' \item{pits}{ Logical vector. Location of the pits in the time series without ex-aequos}
#' \item{tppos}{ Position of the turning points in the initial series (with ex-aequos) }
#'
#'
#' @author
#' This is a stripped-down version taken from package \pkg{pastecs},
#' original authors are Frédéric Ibanez, Philippe Grosjean.
#' The original code and the derived version are licensed under the GPL (>=2).
#'
#' @references
#' Ibanez, F. (1982) Sur une nouvelle application de la theorie de
#'   l'information a la description des series chronologiques planctoniques.
#'   J. Exp. Mar. Biol. Ecol., 4:619-632
#'
#' Grosjean, P. and Ibanez, F. (2014) pastecs: Package for Analysis
#'   of Space-Time Ecological Series. R package version 1.3-18.
#'   https://CRAN.R-project.org/package=pastecs
#'
#' @export
#'
turnpoints <- function(x) {
  data <- deparse(substitute(x))
  if (is.null(ncol(x)) == FALSE)
    stop("Only one series can be treated at a time")
  x <- as.vector(x)
  n <- length(x)
  diffs <- c(x[1] - 1, x[1:(n - 1)]) != x
  uniques <- x[diffs]
  n2 <- length(uniques)
  poss <- (1:n)[diffs]
  exaequos <- c(poss[2:n2], n + 1) - poss - 1
  if (n2 < 3) {			# We need at least 3 unique values
    warning("Less than 3 unique values, no calculation!")
    nturns <- NA
    firstispeak <- FALSE
    peaks <- rep(FALSE, n2)
    pits <- rep(FALSE, n2)
    tppos <- NA
    proba <- NA
    info <- NA
  } else {
    # The following code is faster in R, but does not work all the time!
    #	ex <- embed(uniques, 3)	# Works only in R!
    #	peaks <- c(FALSE, max.col(ex) == 2, FALSE)
    #	pits <- c(FALSE, max.col(-ex) == 2, FALSE)
    m <- n2 - 2
    ex <- matrix(uniques[1:m + rep(3:1, rep(m, 3)) - 1], m)
    peaks <- c(FALSE, apply(ex, 1, max, na.rm = TRUE) == ex[, 2], FALSE)
    pits <- c(FALSE, apply(ex, 1, min, na.rm = TRUE) == ex[, 2], FALSE)
    tpts <- peaks | pits
    if (sum(tpts) == 0) {	# No turning point
      nturns <- 0
      firstispeak <- FALSE
      peaks <- rep(FALSE, n2)
      pits <- rep(FALSE, n2)
      tppos <- NA
    } else {
      tppos <- (poss + exaequos)[tpts]
      # This way, we consider the last element of duplicates, as in PASSTEC 2000
      tptspos <- (1:n2)[tpts]
      firstispeak <- tptspos[1] == (1:n2)[peaks][1]
      nturns <- length(tptspos)
    }
  }
  res <- list(data = data, n = n, points = uniques, pos = (poss + exaequos),
    exaequos = exaequos, nturns = nturns, firstispeak = firstispeak,
    peaks = peaks, pits = pits, tppos = tppos)
  class(res) <- c("list", "abrTurnpoints")
  res
}
