## ----opts, echo = FALSE, message = FALSE---------------------------------
library("knitr")
library("bbmle")
#knitr::opts_chunk$set(eval = FALSE)
knitr::opts_chunk$set(fig.width=6, fig.height=4)

## ----eval=TRUE, echo=FALSE, results="hide"-------------------------------
suppressMessages(require("antibioticR"))

## ----eval=FALSE----------------------------------------------------------
#  library("antibioticR")

## ------------------------------------------------------------------------
## raw data contain NA values
data(micdata)
na.omit(micdata)
plot(freq ~ log2(conc), data=micdata, type="h")

## ------------------------------------------------------------------------
## discard NA values
measured <- na.omit(micdata)

## cumulative plot
plot(cumsum(freq) ~ log2(conc), data=measured, type="l")

## ------------------------------------------------------------------------
x <- log2(measured$conc)
y <- measured$freq

## heuristic start values
pstart <- ecoffinder_startpar(x, y)
pstart

## ------------------------------------------------------------------------
## nonlinear regression
p <- ecoffinder_nls(x, y, pstart, plot=FALSE)
summary(p)

## ---- fig.width=6, fig.height=4------------------------------------------
plot(p)

## ------------------------------------------------------------------------
plot(p, cumulative=FALSE, fits="best")

## ------------------------------------------------------------------------
coef(p)
# abr_quantile(p, q=c(0.01, 0.1, 0.5, 0.9, 0.99)) # not yet implemented, needs log2_flag

## ------------------------------------------------------------------------
breaks <- 0:28
counts <- c(36, 0, 2, 3, 4, 8, 9, 14, 10, 9, 3, 1, 1, 2,
            4, 8, 20, 45, 40, 54, 41, 22, 8, 3, 3, 0, 0,0)

observations <- unbin(breaks[-1], counts) # upper class boundaries

(comp <- mx_guess_components(observations, bw=2/3, mincut=0.9))

obj <- mxObj(comp, left="e")

obj2 <- mx_metafit(breaks, counts, obj)

## ------------------------------------------------------------------------
mx_plot(obj2, disc=5.5, main="", xlab="ZD (mm)")

## ------------------------------------------------------------------------
summary(obj2)

## ------------------------------------------------------------------------
results(obj2)

