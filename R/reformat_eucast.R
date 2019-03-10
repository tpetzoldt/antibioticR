#' Reformat EUCAST crosstabe format into data base formats
#'
#' @param data data frame representing an EUCAST cross table or a result from
#'   this function with "unbin=FALSE"
#' @param type either "ZD" or "MIC", the latter is not implemented yet
#' @param melt if \code{TRUE}, convert EUCAST crosstable to data base format
#' @param unbin expand binned data to single observtions
#'
#' @details
#'
#' Reformatting of from crosstable to data base format is done with
#' function \code{melt} from package \pkg{reshape2}. In contrast,
#' \code{unbin} is a quite memory consuming workaround for R functions that do
#' not support a weight argument. Please consider to use weights and avoid
#' \code{unbin} whenever possible.
#'
#' @note This function is under development, name and functionality may change.
#'
#' @return reformatted data frame
#'
#' @export
#'
#'
#'
reformat_eucast <- function(data, type=c("ZD", "MIC"), melt=TRUE, unbin=FALSE) {
  type <- match.arg(type)
  if (type=="MIC") stop("Sorry, conversion of MIC tables not yet implemented.")
  if (melt == FALSE & unbin == FALSE) warning("data not reformatted")

  ## todo: check data argument: table format and column names

  if (melt){
    melted_euc <- melt(data, id="Antibiotic", value.name = "Isolates")
    zd <- melted_euc[grep("X.*", as.character(melted_euc$variable)),]
    zd$Diameter <- as.numeric(sub("X", "", zd$variable))
    zd$variable <- NULL
  } else {
    zd <- data
  }
  if (unbin) {
  zdx <- unbin(zd, zd$value)
  data.frame(
    #Isolate = "-",
    #Origin = "Eucast",
    Antibiotic = zdx$Antibiotic,
    Diameter = zdx$Diameter
  )
  } else {
    zd[c("Antibiotic", "Diameter", "Isolates")]
  }
}
