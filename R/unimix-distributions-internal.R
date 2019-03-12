## internal versions of the univariate mixture density and distribution functions


.dunimix <- function(x, parms, full.out=FALSE) {
  fun <- function(pp) {
    with(pp, {
      if (type=="n") {
        L * dnorm(x, mean=mean, sd=sd)
      } else if (type=="g") {
        L * dgamma(x, shape=shape, rate=rate)
      } else if (type=="e") {
        L * dexp(x, rate=rate)
      } else {
        stop("distribution type not available")
      }
    })
  }

  ## normalize mixture to 1.0
  LL <- sapply(parms, function(x) x$L)
  LL <- LL/sum(LL)
  for (i in 1:length(LL)) parms[[i]]$L <- LL[i]

  ## compose mixture
  yy <- sapply(parms, fun)
  if (is.matrix(yy)) {
    ret <- rowSums(yy)
  } else {
    ret <- sum(yy)
  }

    ## return density only or a matrix with all components
  if (full.out) {
    ret <- cbind(x=x, yy, y=ret)
  }
  return(ret)
}

.punimix <- function(q, parms, full.out=FALSE) {
  fun <- function(pp) {
    with(pp, {
      if (type=="n") {
        L * pnorm(q, mean=mean, sd=sd)
      } else if (type=="g") {
        L * pgamma(q, shape=shape, rate=rate)
      } else if (type=="e") {
        L * pexp(q, rate=rate)
      } else {
        stop("distribution type not available")
      }
    })
  }
  ## normalize mixture to 1.0
  LL <- sapply(parms, function(x) x$L)
  LL <- LL/sum(LL)
  for (i in 1:length(LL)) parms[[i]]$L <- LL[i]

  ## compose mixture
  yy <- sapply(parms, fun)
  if (is.matrix(yy)) {
    ret <- rowSums(yy)
  } else {
    ret <- sum(yy)
  }

  ## return p only, or p of all components
  if (full.out) {
    ret <- cbind(q=q, yy, y=ret)
  }
  return(ret)
}

.runimix <- function(n, parms, full.out=FALSE) {
  fun <- function(pp) {
    with(pp, {
      if (type=="n") {
        L * rnorm(n, mean=mean, sd=sd)
      } else if (type=="g") {
        L * rgamma(n, shape=shape, rate=rate)
      } else if (type=="e") {
        L * rexp(n, rate=rate)
      } else {
        stop("distribution type not available")
      }
    })
  }
  ## normalize mixture to 1.0
  LL <- sapply(parms, function(x) x$L)
  LL <- LL/sum(LL)
  for (i in 1:length(LL)) parms[[i]]$L <- unname(LL[i])

  ## compose mixture
  yy <- sapply(parms, fun)
  if (is.matrix(yy)) {
    ret <- rowSums(yy)
  } else {
    ret <- sum(yy)
  }

  ## return density only or a matrix with all components
  if (full.out) {
    ret <- cbind(yy, y=ret)
  }
  return(ret)
}
