#' Convert Parameters in data frame format to a paremeter list ...
#'
#' @param df data frame with parameters
#'
#' @return
#'
#' @details deprecated !!!
#'
#' @export
#'
#'
mixParmDf2parmlist <- function(df) {

  ## convert **rows** to lists
  pL <- apply(df, 1, as.list)

  ## convert "numeric" elements back to numeric, but not type
  pL <- lapply(pL, function(L) {
    L[c("mean", "sd", "L")] <- as.numeric(L[c("mean", "sd", "L")])
    L
  })

  ## convert parameters of exponential component
  if (pL[[1]]$type =="e") {
    pL[[1]]$rate <- 1/pL[[1]]$sd
    ## drop redundant parameters
    pL[[1]]$mean <- pL[[1]]$sd <- NULL
  }
  pL
}
