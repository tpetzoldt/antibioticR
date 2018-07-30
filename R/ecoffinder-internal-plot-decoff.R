## internal functions
##    - decoff: density
##    - pecoff: probability

plot_decoff <- function(obj, fits, ...) {

  shift <- 0.5 # !!! hard coded, works only for log2


  with(as.list(obs(obj)), plot(conc, count, type="n", ...))
  abline(h=0, col="gray")

  rng <- range(obj@data$conc)
  x <- seq(rng[1], rng[2], length.out=400)

  if(fits == "all") with(as.list(startpar(obj)),
                         lines(x, dnorm(x, mean=mean, sd=sd) * K, col="gray"))

  ## plot fitted models
  if(fits == "all") {
    lapply(models(obj),
           function(m) {
             p <- coef(m)
             lines(x, dnorm(x, mean=p["mean"], sd=p["sd"]) * p["K"],
                   col="blue", lty="dotted")
           }

    )
  }

  ## best model, quantiles, and used data subset
  #with(obj, {
    p <- coef(obj@fit)
    lines(x, dnorm(x, mean=p["mean"], sd=p["sd"]) * p["K"], col="red", lwd=2)

    ## mark points of best subset with filled circle
    pch <- c(1, 16)[1 + (obs(obj)$conc <= obj@concentrations[obj@i_best])]
    ## replace all empty data points with "x"
    pch <- ifelse(obs(obj)$notempty, pch, 4)

    # !!! shift
    points(obs(obj)$conc - shift, obs(obj)$count, pch=pch, cex=1.5)

    K <- coef(obj)["K"]
    Q <- sub("Q_", "", names(ecoff(obj)))

    Kscaled <- K * dnorm(0)

    if (obj@log2) {
      abline(v=log2(ecoff(obj)), col="red", lty="dashed")
      text(log2(ecoff(obj)), Kscaled - (0:3)*0.8 * Kscaled/4, Q, adj=c(0,-0.5), srt=90)
    } else {
      abline(v=ecoff(obj), col="red", lty="dashed")
      text(ecoff(obj), Kscaled - (0:3)*0.8 * Kscaled/4, Q, adj=c(0,-0.5), srt=90)
    }

    xax <- par("usr")[c(1,2)]
    axis(1, at=seq(trunc(min(xax)), ceiling(max(xax)), 1),
         labels=FALSE, tick=TRUE, tcl=-0.25)
  #})

  if (fits == "all") {
    legend("topleft",
           legend=c("wild type", "excluded subset", "not measured",
                    "initial distribution", "best fit", "other fits",
                    "ecoff quantiles"),
           pch = c(16, 1, 4, NA, NA, NA, NA), bty="n",
           col = c("black", "black", "black", "gray", "red", "blue", "red"),
           lty = c(NA, NA, NA, "solid", "solid", "dotted", "dashed")
    )
  } else {
    legend("topleft",
           legend=c("wild type", "excluded subset", "not measured",
                    "best fit", "ecoff quantiles"),
           pch = c(16, 1, 4,  NA, NA),  bty="n",
           col = c("black", "black", "black", "red", "red"),
           lty = c(NA, NA, NA, "solid", "dashed")
    )
  }
}

