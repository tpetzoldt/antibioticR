#' Identify Peaks in Multimodal Density Curves and Time Series
#'
#' This function identifies peaks in multimodal empirical density functions or in
#' time series and helps to identify the time window of the first maximum according
#' to a given heuristics.
#'
#' @param x x coordinate of a set of points.
#' @param y y coordinate of a set of points. Alternatively, a single
#'   argument x can be provided.
#' @param xstart start before the maximum value of the searched peak (this is a ``weak'' limit)
#' @param xend maximum of the end of the searched peak (this is a ``hard'' maximum)
#' @param minpeak minimum value of the total maximum which is regarded as peak
#' @param mincut minimum relative height of a pit compared to the lower of the
#'   two neighbouring maxima at which these maxima are regarded as separate peaks
#'   (default value is derived from golden section)
#'
#' @return
#'
#' A list with the following elements:
#' \item{peaks}{a data frame with the characteristics (index, xleft, x, xright and y) of all identified peaks,}
#' \item{first.max.index}{index of the maximum value of the ``specified'' peak,}
#' \item{first.max.x}{x-value of the maximum of the ``specified'' peak,}
#' \item{first.indices}{indices (data window) of all data belonging to the ``specified'' peak,}
#' \item{first.x}{x-values (time window) of all data belonging to the ``specified'' peak,}
#' \item{first.y}{corresponding y-values of all data belonging to the ``specified'' peak,}
#' \item{peakid}{vector with peak-id-numbers for all data.}
#'
#' @export
#'
#' @examples
#'
#' ## generate test data with 3 peaks
#' set.seed(123)
#' x <- seq(0, 360, length = 20)
#' y <- abs(rnorm(20, mean = 1, sd = 0.1))
#' y[5:10] <- c(2, 4, 7, 3, 4, 2)
#' y <- c(y, 0.8 * y, 1.2 * y)
#' x <- seq(0, 360, along = y)
#' y[6] <- y[7]   # test case with 2 neighbouring equal points
#' ## plot the test data
#' plot(x, y, type="b")

#' ## identify the first peak
#' peaks <- peakwindow(x, y)
#' ind <- peaks$smd.indices
#' lines(x[ind], y[ind], col="red", lwd=2)
#'
#' ## some more options ...
#' peaks <- peakwindow(x, y, xstart=150, mincut = 0.455)
#' ind <- peaks$first.indices
#' lines(x[ind], y[ind], col = "blue")
#' points(x, y, col = peaks$peakid +1, pch = 16) # all peaks
#'
#' ## work with indices only
#' peaks <- peakwindow(y)
#'
#' ## test case with disturbed sinus
#' x<- 1:100
#' y <- sin(x/5) +1.5 + rnorm(x, sd = 0.2)
#' peaks <- peakwindow(x, y)
#' plot(x, y, type = "l", ylim = c(0, 3))
#' points(x, y, col = peaks$peakid + 2, pch = 16)
#'
#' ## test case: only one peak
#' yy <- c(1:10, 11:1)
#' peakwindow(yy)
#'
#' ## error handling test case: no turnpoints
#' # yy <- rep(1, length(x))
#' # peakwindow(x, yy)
#'
#'
peakwindow <- function(x, y = NULL, xstart = 0, xend = max(x),
                        minpeak = 0.1, mincut = 0.382) {
  ## local function for identifying start and end of all peaks
  ## does also numbering of peaks (id-number)
  numpeaks <- function(ft, y) {
    npeaks <- length(ft)
    ndata  <- length(y)
    if (npeaks > 0) {
      fp <- x1 <- x2 <- numeric(npeaks)
      peakid <- numeric(length(y))
      for (i in 1:npeaks) {
        fp[i] <- ft[i]
        mp <- match(fp[i], ft)
        if (mp == 1)      x1[i] <- 1     else x1[i] <- ft[mp-1] + which.min(y[ft[mp-1]:fp[i]])-1
        if (mp == npeaks) x2[i] <- ndata else x2[i] <- fp[i]    + which.min(y[fp[i]:ft[mp+1]])-1
        peakid[x1[i]:x2[i]] <- i
      }
    } else {
      fp <- ft
      x1 <- x2 <- NULL
      peakid <- rep(0, ndata)
    }

    list(fp=fp, x1=x1, x2=x2, id=peakid)
  }

  ## start of main algorithm
  xy <- xy.coords(x, y)
  x <- xy$x
  y <- xy$y

  ## Technical note: xend = max(x) is a "promise".
  ## It is evaluated *now* with the new x after xy.coords
  iend <- max(c(1, which(x <= xend)))
  tp   <- turnpoints(y)
  ft   <- tp$tppos

  ## default return value in case of no turnpoints
  if (all(is.na(ft))) {
    x1 <- 1
    x2 <- length(y)
    fp <- NULL
    ft <- NULL
  } else {
    ## Determination of all peaks
    ft   <- ft[seq(2 - 1 * tp$firstispeak, length(ft), by = 2)]
    ## Add first point if it is a peak
    if (y[1] > y[2]) ft <- c(1, ft)
    ## Add last point if it is a peak
    if (y[iend] > y[iend - 1]) ft <- c(ft, iend)
    ## Limitation until index iend (e.g. iend = mid of year)
    ft   <- ft[ft <= iend]
    ## Limitation to peaks > minpeak * maxvalue (e.g. 10%)
    ftsmall <- which(y < minpeak * max(y))
    ft      <- ft[!ft %in% ftsmall]
    ## If more than one peak then search ...
    dosearch <- ifelse(length(ft) > 1, TRUE, FALSE)
    while(dosearch) {
      fl  <- length(ft)
      eli <- NULL
      for (n in 1:(fl - 1)) {
        ## Minimum between 2 peaks
        km <- which.min(y[ft[n:(n+1)]])
        ## Heuristics:
        ## if Minimum between 2 peaks > mincut of the smaller neighbours
        ## then eliminate peak
        if (min(y[ft[n:(n+1)]]) * mincut < min(y[ft[n]:ft[n+1]]))
        eli <- c(eli, n + km - 1)
      }
      if (length(eli) == 0) dosearch <- FALSE else ft <- ft[-eli]
      if (length(ft) < 2)   dosearch <- FALSE
    }
    ## Search as long as
    ## - no peak can be eliminated or
    ## - only one peak left over
    if (length(ft) > 1) {
      ## If more than one peak
      ## take the first one after xstart
      ## If no peak xstart, then take the last existing, even if before xstart
      if (any(x[ft] > xstart)) {
        fp <- min(ft[x[ft]  > xstart])
      } else {
        fp <- max(ft[x[ft] <= xstart])
      }
      mp <- match(fp, ft)
      if (mp == 1)          x1 <- 1  else x1 <- ft[mp-1] + which.min(y[ft[mp-1]:fp])-1
      if (mp == length(ft)) x2 <- iend else x2 <- fp     + which.min(y[fp:ft[mp+1]])-1
    } else {
      fp <- ft
      x1 <- 1
      x2 <- iend
    }
  }
  allpeaks <- numpeaks(ft, y)
  ## Output: - all peak maxima as data frame
  ##         - Start of data window, peak, end of window
  ##           given as indices, x and y values

  ret <-  list(peaks = data.frame(
                         index = ft,
                         xleft = allpeaks$x1,
                         x     = x[ft],
                         xright= allpeaks$x2,
                         y     = y[ft]
                       ),
               # data   = data.frame(x=x, y=y),
               first.max.index = fp,
               first.max.x     = x[fp],
               first.indices   = x1:x2,
               first.x         = x[x1:x2],
               first.y         = y[x1:x2],
               peakid          = allpeaks$id
  )
  class(ret) <- c("list", "abrPeakwindow")
  ret
}

