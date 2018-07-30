## internal functions
##    - decoff: density
##    - pecoff: probability


plot_pecoff <-function(obj, fits, ...) {

  with(obs(obj), plot(conc, cumCount, type="n", ...))

  rng <- range(obs(obj)$conc)
  x <- seq(rng[1], rng[2], length.out=100)

  with(as.list(startpar(obj)), lines(x, pnorm(x, mean=mean, sd=sd) * K, col="gray"))

  ## plot fitted models
  lapply(models(obj),
         function(m)
           lines(x, predict(m, newdata=list(conc=x)), col="blue", lty="dotted")
  )

  ## best model, quantiles, and used data subset
  #with(obj, {
    lines(x, predict(fit(obj), newdata=list(conc=x)), col="red", lwd=2)

    ## mark points of best subset with filled circle
    pch <- c(1, 16)[1 + (obs(obj)$conc <= obj@concentrations[obj@i_best])]
    ## replace all empty data points with "x"
    pch <- ifelse(obs(obj)$notempty, pch, 4)

    points(obs(obj)$conc, obs(obj)$cumCount, pch=pch, cex=1.5)

    K <- coef(obj)["K"]
    Q <- sub("Q_", "", names(ecoff(obj)))


    if (obj@log2) {
      abline(v=log2(ecoff(obj)), col="red", lty="dashed")
      text(log2(ecoff(obj)), (0:3)*0.8 * K/4, Q, adj=c(0,-0.5), srt=90)
    } else {
      abline(v=ecoff(obj), col="red", lty="dashed")
      text(ecoff(obj), (0:3)*0.8 * K/4, Q, adj=c(0,-0.5), srt=90)
    }

    xax <- par("usr")[c(1,2)]
    axis(1, at=seq(trunc(min(xax)), ceiling(max(xax)), 1), labels=FALSE, tick=TRUE, tcl=-0.25)
  #})

  if (fits == "all") {
    legend("topleft",
           legend=c("wild type", "excluded subset", "not measured",
                    "initial distribution", "best fit", "other fits", "ecoff quantiles"),
           pch = c(16, 1, 4, NA, NA, NA, NA), bty="n",
           col = c("black", "black", "black", "gray", "red", "blue", "red"),
           lty = c(NA, NA, NA, "solid", "solid", "dotted", "dashed")
    )
  } else {
    legend("topleft",
           legend=c("wild type", "excluded subset", "not measured", "best fit", "ecoff quantiles"),
           pch = c(16, 1, 4,  NA, NA), bty="n",
           col = c("black", "black", "black", "red", "red"),
           lty = c(NA, NA, NA, "solid", "dashed")
    )
  }
}
