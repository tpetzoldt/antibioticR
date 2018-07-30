#' Analysis of Antbiotic Resistance Data
#'
#' A collection of methods ...
#'
#'
#' @name antibioticR-package
#' @aliases antibioticR antibioticR-package
#' @docType package
#' @author Thomas Petzoldt
#'
## @seealso
#'
#' @references
#'
#' Bolker, Ben and R Development Core Team (2017) bbmle:
#'   Tools for General Maximum Likelihood Estimation. R package version 1.0.20.
#'   \url{https://CRAN.R-project.org/package=bbmle}
#'
#' Gruen, Bettina and Leisch, Friedrich (2008) FlexMix Version 2: Finite
#'   mixtures with concomitant variables and varying and constant parameters.
#'   Journal of Statistical Software, 28(4), 1-35 \doi{10.18637/jss.v028.i04}
#'
#' The European Committee on Antimicrobial Susceptibility Testing (2018).
#'    Breakpoint tables for interpretation of MICs and zone diameters, version 8.0,
#'    \url{http://www.eucast.org/clinical_breakpoints/} accessed: 2018-07-09
#'
#' Turnidge, J., Kahlmeter, G., Kronvall, G. (2006) Statistical characterization of
#'   bacterial wild-type MIC value distributions and the determination of
#'   epidemiological cut-off values. Clin Microbial Infect 12: 418-425
#'   \doi{10.1111/j.1469-0691.2006.01377.x}
#'
#' Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S.
#'   Fourth Edition. Springer, New York. ISBN 0-387-95457-0
#'
#'
#'
#' @keywords package
#'
## @examples
#'
#'
#' @import stats graphics grDevices
#' @import methods
#'
## @importClassesFrom bbmle mle2
## @importMethodsFrom bbmle summary coef
#'
#' @importFrom utils read.table
#' @importFrom reshape2 melt
#' @importFrom evmix dbckden
#' @importFrom evmix pbckden
#' @importFrom bbmle mle2
## @importFrom parallel makeCluster stopCluster parLapply
#'
## suggest: mixtools, flexmix

NULL
