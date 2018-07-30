---
title: "Analysis of Antibiotic Resistance Tests with Package `antibioticR`"
author: "Thomas Petzoldt"
date: "2018-07-30"
output: rmarkdown::html_vignette
bibliography: antibioticR.bib
vignette: >
  %\VignetteIndexEntry{Estimation of Antibiotic MIC and ZD Data}
  %\VignetteEngine{knitr::rmarkdown}
---




<!--
possible output options are:
rmarkdown::pdf_document
rmarkdown::html_vignette

set to UTF if necessary with:
%\VignetteEncoding{UTF-8}

Bibliographies may need to set the LANG environment variable
LANG=en_US.UTF-8
-->

**Please note:** This document reflects work in progress and will be updated 
from time to time. The most recent copy can be downloaded from https://github.com/tpetzoldt/

Introduction
============

During the 20th century, antimicrobial agents like antibiotics (that are active agains bacteria), 
antivirals, antifungals and other substances that are active against parasites (e.g. against malaria) [@amr2016]
have made modern medicine possible. Hover, the dramatic increase of resistant and multiresistant bacterial pathogens
is now been widely recognized as a global challenge for human health [@Roca2015,@amr2016,@ECDC2017].

It there is currently still lack in understanding how environmental factors influence evolution and transmission of antibiotic resistances, "global efforts are required to characterize and quantify antibiotic resistance in the environment" [@Berendonk2015].

This package aims to improve the accesibility to statistical methods for analysing populations of resistant and non-resistant bacteria from an environmental, i.e. non-clinical perspective. The methods are intended to describe sensitivity, tolerance and resistance on a sub-acute level in order to compare populations of different origin on gradual scales.

We assume that environmental populations are composed of different geno- and phenotypes, so that quantitative data from standard methods like disk diffusion zone diameters (ZD) or minimum inhibitory concentration (MIC) values will yield multi-modal univariate mixture distributions when tested against single antibiotics.

The package relies on existing packages, especially packages  **evmix** [@Hu2018] for boundary corrected density estimation and package **bbmle** [@Bolker2017] for maximum likelihood estimation. The package will be amended by visualization tools and interactive web-applications using R's base graphics and statistics packages [RCore2015], packages **ggplot2** [@Wickham2016] and **shiny** [@Chang2018].

**Please note:** The package ant this document are in an early stage of development (pre-alpha). It comes without warranty and is **not intended for clinical applications**. Its functions and classes are likely to change and may contain mistakes and errors. Source code of the development version is available from <https://github.com/tpetzoldt>. Comments are welcome.




Methods
=======

The package includes three types of methods:

The package supports currently three methods

1. Kernel density smoothing for getting mean values and multiple modes from the distributions,
2. An **R** implementation of the ECOFFinder [@Turnidge2006]  with automatic start value 
  estimation and a shiny app for interactive use,
3. Maximum likelihood estimation of multi-modal normal and exponential-normal mixtures.


Data set
========

The data set for demonstrating main features of the package was
provided by ... It contains ...

** the pre-alpha version contains articficial test data** 




After loading the package:


```r
library("antibioticR")
```

we load the data and inspect its structure with `str`:


...

Identification of the wild-type population
==========================================

The wild type population is defined as the population ...

ECOFFinder
----------

ECOFFinder is an algorithm developed by @Turnidge2006. Wild-type cut-off values 
for MIC data are estimated from quantiles of a normal distribution of $\log_2$ 
transformed MIC values. Mean and standard deviation of this distribution are
estimated by non-linear regression between a normal probability distribution 
(that is scaled in y-direction) and cumulative frequencies of observation data. In order to 
separate the wild-type normal distribution from other components, e.g. the 
resistant sub-population, the regression is repeated using successive subsets of 
data, until an optimal fit between the subset and the normal probability 
function is found.

Firstly we load and inspect the test data set, included in the package:


```r
## raw data contain NA values
data(micdata)
head(micdata)
```

```
##          conc freq
## 1 0.000976563   NA
## 2 0.001953125   NA
## 3 0.003906250   NA
## 4 0.007812500   NA
## 5 0.015625000   NA
## 6 0.031250000    1
```

```r
plot(freq ~ log2(conc), data=micdata, type="h")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

Then we omit not measured (NA) values, transform the data and plot the cumulative distribution:


```r
## discard NA values
measured <- na.omit(micdata)

## cumulative plot
plot(cumsum(freq) ~ log2(conc), data=measured, type="l")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

Now we copy the transformed data to new variables `x`and `y` to save typing. 
Function `ecoffinder_startpar` then helps us to guess start parameters for the 
subsequent nonlinear regression. The function works internally by applying a 
kernel density estimation. Details can be found on the help page of 
`ecoffinder_startpar` and of R's function `density`:


```r
x <- log2(measured$conc)
y <- measured$freq

## heuristic start values
pstart <- ecoffinder_startpar(x, y)
pstart
```

```
##       mean         sd          K 
##  -1.002944   1.161222 338.000000
```

If the start parameters look reasonable, they can be directly fed into ecoffinder_nls`. 
This works well in many cases, but sometimes, it may be necessary to enter 
user-defined start parameters instead. 


```r
## nonlinear regression
p <- ecoffinder_nls(x, y, pstart, plot=FALSE)
```

```
## Search concentration:  0 1 2 3 4 5
```

```r
summary(p)
```

```
## 
## Formula: cumCount ~ fnorm(conc, mean, sd, K)
## 
## Parameters:
##       Estimate Std. Error t value Pr(>|t|)    
## mean  -1.54565    0.04257  -36.31 2.91e-08 ***
## sd     0.91042    0.05768   15.78 4.10e-06 ***
## K    339.02677    4.00460   84.66 1.83e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.179 on 6 degrees of freedom
## 
## Number of iterations to convergence: 8 
## Achieved convergence tolerance: 8.872e-06
## 
## ---
## ECOFF quantiles:
## Note: log2 used for the transformation of MIC data! Q_0.95 Q_0.975  Q_0.99 Q_0.999 
##       1       2       2       4
```
A visualisation is possible during the fitting process (`plot=TRUE`) or with a 
specialized plot function:


```r
plot(p)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

```
## Note: log2 used for the transformation of MIC data!Note: log2 used for the transformation of MIC data!Note: log2 used for the transformation of MIC data!
```


```r
plot(p, cumulative=FALSE, fits="best")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

```
## Note: log2 used for the transformation of MIC data!Note: log2 used for the transformation of MIC data!Note: log2 used for the transformation of MIC data!
```


Additional functions like `coef` or `abr_quantile` can be used to access additional results


```r
coef(p)
```

```
##        mean          sd           K 
##  -1.5456547   0.9104227 339.0267707
```

```r
# abr_quantile(p, q=c(0.01, 0.1, 0.5, 0.9, 0.99)) # not yet implemented, needs log2_flag
```


Mixture approach
----------------



```r
breaks <- 0:28
counts <- c(36, 0, 2, 3, 4, 8, 9, 14, 10, 9, 3, 1, 1, 2,
            4, 8, 20, 45, 40, 54, 41, 22, 8, 3, 3, 0, 0,0)

observations <- unbin(breaks[-1], counts) # upper class boundaries

(comp <- mx_guess_components(observations, bw=2/3, mincut=0.9))
```

```
##        mean       sd         L
## 1  1.040054 1.002270 0.1055751
## 2  7.805322 2.456927 0.1775396
## 3 19.505968 2.192820 0.7168853
```

```r
obj <- mxObj(comp, left="e")

obj2 <- mx_metafit(breaks, counts, obj)

mx_plot(obj2, breaks, counts, disc=5.5)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

```r
## simplification for fitted objects with save.data = TRUE

mx_plot(obj2, disc=5.5, main="Univariate Mixture Distribution-Approach", xlab="ZD (mm)")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-2.png)




Fiting multiple data sets
=========================

Finally
=======


Acknowledgments
===============

Many thanks to ...... for the data set, to ...... for stimulation and discussion and
to the R Core Team [@RCore2015] for developing and maintaining **R**. 
This documentation was written using **knitr** [@knitr2014] and **rmarkdown**
[@rmarkdown].


References
==========

