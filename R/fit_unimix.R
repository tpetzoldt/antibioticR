#' Fit of Normal and Exponential-Normal Mixtures to Binned Data
#'
#' Fit mixture distributions to binned data with a maximum likelihood method,
#'   inspired by Venables and Ripley (2002)
#'
#' @param breaks upper class limits of the data
#' @param counts frequency of observations
#' @param parms list of initial parameters for the mixture
#' @param type of the mixture distribution, e.g. 'enn' for exponential-normal-normal
#' @param ... additional arguments passed to \code{mle2}
#'
#' @return
#'
#' @references
#'
#'    Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S.
#'    Fourth Edition. Springer, New York. ISBN 0-387-95457-0
#'
#'    Bolker, Ben and R Development Core Team (2017) bbmle:
#'    Tools for General Maximum Likelihood Estimation. R package version 1.0.20.
#'    \url{https://CRAN.R-project.org/package=bbmle}
#'
#'
#' @examples
#'

#' @rdname fit_unimix
#' @export
#'
fit_unimix <- function(breaks, counts, parms,
  type=c("en", "enn", "ennn", "ennnn", "n", "nn", "nnn", "nnnn", "nnnnn"), ...) {

  # todo:
  ## argument checking
  ## automatical creation of other types
  type <- paste0("fit_", match.arg(type))

  do.call(type, list(breaks=breaks, counts=counts, parms=parms, ...))
}


#' @rdname fit_unimix
#' @export
#'
fit_n <- function(breaks, counts, parms, ...) {
  llunimix <- function(mean1, sd1) {
    z <- pnorm(breaks, mean1, sd1)
    #return(- sum(counts * log(diff(z))))
    return(- wsum(log(diff(z)), counts))
  }
  parms$L1 <- NULL # in case it exists; L=1 in case of 1 component
  mle2(llunimix, start=parms, ...)
}


## maximum likelihood fit of exponential-normal-mixtures
#' @rdname fit_unimix
#' @export
#'
fit_en <- function(breaks, counts, parms, ...) {
  llunimix <- function(L1, rate1, mean2, sd2) {
    if ((L1 < 0) | (L1 > 1) | rate1 <= 0 | sd2 <=0) {
      ## penalty for wrong parameters
      1e-9 * .Machine$double.xmax
    } else {
      plist <- list(
        e1 = list(type="e", L = L1, rate=rate1),
        n2 = list(type="n", L = 1-L1, mean=mean2, sd=sd2)
      )
      z <- .punimix(breaks, plist)
      return(- wsum(log(diff(z)), counts))
    }
  }
  parms$L2 <- NULL # in case it exists, because last L is difference to 1
  mle2(llunimix, start=parms, ...)
}

## maximum likelihood fit of normal-normal-mixtures
#' @rdname fit_unimix
#' @export
#'
fit_nn <- function(breaks, counts, parms, ...) {
  llunimix <- function(L1, mean1, sd1, mean2, sd2) {
    if ((L1 < 0) | (L1 > 1) | sd1 <=0 | sd2 <=0) {
      ## penalty for wrong parameters
      1e-9 * .Machine$double.xmax
    } else {
      L1 <- max(L1, 0) # force non-negativity
      plist <- list(
        n1 = list(type="n", L = L1, mean=mean1, sd=sd1),
        n2 = list(type="n", L = 1 - L1, mean=mean2, sd=sd2)
      )
      z <- .punimix(breaks, plist)
      return(- wsum(log(diff(z)), counts))
    }
  }
  #parms$L2 <- NULL # in case it exists, because last L is difference to 1
  mle2(llunimix, start=parms, ...)
}


## maximum likelihood fit of exponential-normal-normal-mixtures
#' @rdname fit_unimix
#' @export
#'
fit_enn <- function(breaks, counts, parms, ...) {
  llunimix <- function(L1, L2, rate1, mean2, sd2, mean3, sd3) {
    if (L1 < 0 | L2 < 0 | abs(L1) + abs(L2) > 1 | rate1 <= 0
        | rate1 <= 0 | sd2 <= 0 | sd3 <= 0) { # test: rate or sd <=0
      ## penalty for wrong parameters
      1e-9 * .Machine$double.xmax
    } else {
      #L1 <- max(L1, 0) # force non-negativity
      plist <- list(
        e1 = list(type="e", L = L1, rate=rate1),
        n2 = list(type="n", L = L2, mean=mean2, sd=sd2),
        n3 = list(type="n", L = 1 - L1 - L2, mean=mean3, sd=sd3)
      )
      z <- .punimix(breaks, plist)
      return(- wsum(log(diff(z)), counts))
    }
  }
  parms$L3 <- NULL # in case it exists, because last L is difference to 1
  mle2(llunimix, start=parms, ...)
}

## maximum likelihood fit of normal-normal-normal-mixtures
#' @rdname fit_unimix
#' @export
#'
fit_nnn <- function(breaks, counts, parms, ...) {
  llunimix <- function(L1, L2, mean1, sd1, mean2, sd2, mean3, sd3) {
    if (L1 < 0 | L2 < 0 | abs(L1) + abs(L2) > 1
        | sd1 <=0 | sd2 <=0 | sd3 <=0) {
      ## penalty for wrong parameters
      1e-9 * .Machine$double.xmax
    } else {
      #L1 <- max(L1, 0) # force non-negativity
      plist <- list(
        n1 = list(type="n", L = L1, mean=mean1, sd=sd1),
        n2 = list(type="n", L = L2, mean=mean2, sd=sd2),
        n3 = list(type="n", L = 1 - L1 - L2, mean=mean3, sd=sd3)
      )
      z <- .punimix(breaks, plist)
      return(- wsum(log(diff(z)), counts))
    }
  }
  parms$L3 <- NULL # in case it exists, because last L is difference to 1
  mle2(llunimix, start=parms, ...)
}

## maximum likelihood fit of normal-normal-normal-normal-mixtures
#' @rdname fit_unimix
#' @export
#'
fit_nnnn <- function(breaks, counts, parms, ...) {
  llunimix <- function(L1, L2, L3, mean1, sd1, mean2, sd2, mean3, sd3, mean4, sd4) {
    if (L1 < 0 | L2 < 0 | L3 < 0 | abs(L1) + abs(L2) +abs(L3) > 1
        | sd1 <=0 | sd2 <=0 | sd3 <=0 | sd4 <= 0) {
      ## penalty for wrong parameters
      1e-9 * .Machine$double.xmax
    } else {
      #L1 <- max(L1, 0) # force non-negativity
      plist <- list(
        n1 = list(type="n", L = L1, mean=mean1, sd=sd1),
        n2 = list(type="n", L = L2, mean=mean2, sd=sd2),
        n3 = list(type="n", L = L3, mean=mean3, sd=sd3),
        n4 = list(type="n", L = 1 - L1 - L2 - L3, mean=mean4, sd=sd4)
      )
      z <- .punimix(breaks, plist)
      return(- wsum(log(diff(z)), counts))
    }
  }
  parms$L4 <- NULL # in case it exists, because last L is difference to 1
  mle2(llunimix, start=parms, ...)
}


## maximum likelihood fit of exponential-normal-normal-normal-mixtures
#' @rdname fit_unimix
#' @export
#'
fit_ennn <- function(breaks, counts, parms, ...) {
  llunimix <- function(L1, L2, L3, rate1, mean2, sd2, mean3, sd3, mean4, sd4) {
    if (L1 < 0 | L2 < 0 | L3 < 0 | abs(L1) + abs(L2) + abs(L3) > 1
        | rate1 <= 0 | sd2 <= 0 | sd3 <= 0 | sd4 <= 0
      ) {
      ## penalty for wrong parameters
      1e-9 * .Machine$double.xmax
    } else {
      #L1 <- max(L1, 0) # force non-negativity
      plist <- list(
        e1 = list(type="e", L = L1, rate=rate1),
        n2 = list(type="n", L = L2, mean=mean2, sd=sd2),
        n3 = list(type="n", L = L3, mean=mean3, sd=sd3),
        n4 = list(type="n", L = 1 - L1 - L2 - L3, mean=mean4, sd=sd4)
      )
      z <- .punimix(breaks, plist)
      return(- wsum(log(diff(z)), counts))
    }
  }
  parms$L4 <- NULL # in case it exists, because last L is difference to 1
  mle2(llunimix, start=parms, ...)
}

## maximum likelihood fit of exponential-normal-normal-normal-mixtures
#' @rdname fit_unimix
#' @export
#'
fit_ennnn <- function(breaks, counts, parms, ...) {
  llunimix <- function(L1, L2, L3, L4, rate1, mean2, sd2, mean3, sd3, mean4, sd4, mean5, sd5) {
    if (L1 < 0 | L2 < 0 | L3 < 0 | L4 < 0 | abs(L1) + abs(L2) + abs(L3) + abs(L4) > 1
        | rate1 <= 0 | sd2 <= 0 | sd3 <= 0 | sd4 <= 0 | sd5 <= 0) {
      ## penalty for wrong parameters
      1e-9 * .Machine$double.xmax
    } else {
      #L1 <- max(L1, 0) # force non-negativity
      plist <- list(
        e1 = list(type="e", L = L1, rate=rate1),
        n2 = list(type="n", L = L2, mean=mean2, sd=sd2),
        n3 = list(type="n", L = L3, mean=mean3, sd=sd3),
        n4 = list(type="n", L = L4, mean=mean4, sd=sd4),
        n5 = list(type="n", L = 1 - L1 - L2 - L3 - L4, mean=mean5, sd=sd5)
      )
      z <- .punimix(breaks, plist)
      return(- wsum(log(diff(z)), counts))
    }
  }
  parms$L5 <- NULL # in case it exists, because last L is difference to 1
  mle2(llunimix, start=parms, ...)
}

## maximum likelihood fit of exponential-normal-normal-normal-mixtures
#' @rdname fit_unimix
#' @export
#'
fit_nnnnn <- function(breaks, counts, parms, ...) {
  llunimix <- function(L1, L2, L3, L4, mean1, sd1, mean2, sd2, mean3, sd3, mean4, sd4, mean5, sd5) {
    if (L1 < 0 | L2 < 0 | L3 < 0 | L4 < 0 | abs(L1) + abs(L2) + abs(L3) + abs(L4) > 1
        | sd1 <= 0 | sd2 <= 0 | sd3 <= 0 | sd4 <= 0 | sd5 <= 0) {
      ## penalty for wrong parameters
      1e-9 * .Machine$double.xmax
    } else {
      #L1 <- max(L1, 0) # force non-negativity
      plist <- list(
        n1 = list(type="n", L = L1, mean=mean1, sd=sd1),
        n2 = list(type="n", L = L2, mean=mean2, sd=sd2),
        n3 = list(type="n", L = L3, mean=mean3, sd=sd3),
        n4 = list(type="n", L = L4, mean=mean4, sd=sd4),
        n5 = list(type="n", L = 1 - L1 - L2 - L3 - L4, mean=mean5, sd=sd5)
      )
      z <- .punimix(breaks, plist)
      return(- wsum(log(diff(z)), counts))
    }
  }
  parms$L5 <- NULL # in case it exists, because last L is difference to 1
  mle2(llunimix, start=parms, ...)
}
