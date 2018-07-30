
#' Convert Parameter Vector to List
#'
#' @description deprecated! use as(vector, mxObj instead)
#'
#' @param p parameter vector of the following form: ..............
#'
#' @return list structure of mixture distribution parameters
#'
#' @details works for exponential, normal and gamma distributions
#'
#' @export
#'
#' @examples
#'
parmList <- function(p) {
  nm <- names(p)
  ## identify number of components from number postfix of parameter names
  N <- max(as.numeric(gsub("[a-z,A-Z]+", "", nm)))
  plist <- list()
  for (i in 1:N) {
    ndx <- grep(i, nm)
    tmp <- as.list(p[ndx])
    tmpnames <- names(tmp)
    ## detect type
    if (length(grep("shape", tmpnames))) {
      type <- "g"
    } else if (length(grep("mean", tmpnames))) {
      type <- "n"
    } else if (length(grep("rate", tmpnames))) { # but not shape
      type <- "e"
    } else {
      stop("invalid parameter vector")
    }
    ## remove number postfix
    names(tmp) <- gsub("[0-9]+", "", tmpnames)
    plist[[i]] <- c(type = type, tmp)
  }

  ## assign names to components
  type <- lapply(plist, function(x) x$type)
  names(plist) <- paste0(type, 1:N)

  Lsum <- 0
  if (N > 1){
    ## set last L value to 1 - sum(L_i)
    Lsum <- sum(sapply(1:(N-1), function(i) plist[[i]]$L))
  }
  plist[[N]]$L <- 1 - Lsum

  plist
}

