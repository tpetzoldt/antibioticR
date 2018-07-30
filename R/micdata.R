#' Minimum Inhibitory Concentration (MIC) Data
#'
#' Example data set of a MIC experiment, taken from Turnidge et al. (2006)
#'
#'
#' @format Data frame with two columns:
#' \describe{
#'   \item{conc}{vector of antibiotic concentrations}
#'   \item{freq}{frequency of detected MIC concentrations}
#' }
#'
#' @source \url{https://clsi.org/education/microbiology/ecoffinder/}
#'
#' @references Turnidge, J., Kahlmeter, G., Kronvall, G. (2006) Statistical characterization of
#'   bacterial wild-type MIC value distributions and the determination of
#'   epidemiological cut-off values. Clin Microbial Infect 12: 418-425
#'   doi: 10.1111/j.1469-0691.2006.01377.x

#'
#' @name micdata
#' @docType data
#' @keywords data
#'
#' @examples
#'
#' data(micdata)
#' plot(freq ~ log2(conc), data=micdata, type="h")
#'
#' ## replace NA values with zero
#' micdata$freq[is.na(micdata$freq)] <- 0
#' plot(cumsum(freq) ~ log2(conc), data=micdata, type="l")
#'
#'
NULL
