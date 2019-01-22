# antibioticR

is an **R package** to estimate antibiotic resistance cutoff values from ZD and MIC distributions of environmental samples 

The package aims to improve the accessibility to statistical methods for analyzing populations of resistant and non-resistant bacteria from an environmental, i.e. non-clinical perspective. The methods are intended to describe sensitivity, tolerance and resistance on a sub-acute level in order to compare populations of different origin on gradual scales.

We assume that environmental populations are composed of different geno- and phenotypes, so that quantitative data from standard methods like disc diffusion zone diameters (ZD) or minimum inhibitory concentration (MIC) values will yield multi-modal univariate mixture distributions when tested against single antibiotics.

The package relies on existing packages, especially packages  **evmix** [@Hu2018] for boundary corrected density estimation and package **bbmle** [@Bolker2017] for maximum likelihood estimation. The package will be amended by visualization tools and interactive web-applications using R's base graphics and statistics packages [RCore2015], packages **ggplot2** [@Wickham2016] and **shiny** [@Chang2018].

**Please note:** The package and this document are in an early stage of development (pre-alpha). It comes without warranty and is **not intended for clinical applications**. Its functions and classes are likely to change and may contain mistakes and errors. Comments are welcome.




Methods
-------

The package supports currently three methods:

1. Kernel density smoothing for getting mean values and multiple modes from the distributions,
2. An **R** implementation of the ECOFFinder algorithm [@Turnidge2006]  with automatic start value estimation and a shiny app for interactive use,
3. Maximum likelihood estimation of multi-modal normal and exponential-normal mixtures.


Download and Installation
-------------------------

### Release version

The package is not yet released.


### Development version

Install with package devtools:

    install.packages("devtools")
    library(devtools)
    install_github("tpetzoldt/antibioticR")
	
	
Demo
----

A live demo of the ECOFFinder approach can be found	at:

http://limno-live.hydro.tu-dresden.de/ecoffinder/

Documentation
-------------

The documentation is still to be written. An initial version is found here:

https://rawgit.com/tpetzoldt/antibioticR/master/vignettes/Introduction.html

