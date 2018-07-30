#
#' Sort and Select Distribution Components
#'
#' @param p   mixture distribution parameter list
#' @param N number of components
#' @param item parameter name used for sorting and selection (e.g. 'L', 'mean', 'sd')
#' @param ascending ascending  (TRUE) ode descending (FALSE) order
#' @param as.vector if TRUE, result is returned as a vector, otherwise as list
#' @param ... reserved for future extensions
#'
#' @return
#'
#' @examples
#'
#' @rdname mx_parmlist_sort
#'
#' @export
#'
mx_parmlist_sort <- function(p) {
  ## sort parameter *within* sublists

  if(is(p, "mxObj")) p <- coef(p)

  ord <- c("type", "L", "rate", "shape", "mean", "sd")
  for (i in 1:length(p)) {
    nm <- names(p[[i]])
    ## bring parameters in standard order
    r <- rank(match(ord, nm), na.last=NA)
    p[[i]] <- p[[i]][r]
  }
  names(p) <- paste0(sapply(p, function(x) x$type), 1:length(p))
  p
}

## find top N list elements according to a statistical parameter ("item')
#' @rdname mx_parmlist_sort
#'
#' @export
#'
mx_parmlist_select <- function(p, N=1, item=c("L", "mean", "sd"), ascending = TRUE) {

  par <- match.arg(par)
  ord <- ifelse(ascending, 1L, -1L)

  ## p can be an mxObj-ect or a parameter list
  #if(isClass("mxObj", p)) p <- coef(p)

  ## append dummy mean parameter to exponential
  ## works only for a single exponential component
  p_temp <- p
  if (par %in% c("mean", "sd")) {
    ndx <- (sapply(p, function(x) x$type) == "e")
    elem <- p[ndx][[1]]
    if (elem$type=="e") {
      mean <- sd <- 1/elem$rate # parameter relationship of exponential distribution
      p_temp[ndx][[1]] <- c(elem, mean=mean, sd=sd)
    }
  }

  ## collect all weights in vector
  y <- sapply(p_temp, function(x) x[[item]])

  ## find the N best (or worst)
  ## uses the original list p, not the augmented p_temp
  p_new <- p[whichpart(ord * y, N)]
  new("mxObj", coef=p_new)
}


## todo: sort distr, e, g, n and then with decreasing mean, L or sd

#' @rdname mx_parmlist_sort
#'
#' @export
#'
mx_intermediate <- function(p, as.vector = TRUE) {
  ## main normal component with smallest L

  if(isClass("mxObj", p)) p <- coef(p)

  n.norm <- sum(sapply(p, function(x) x$type == "n"))
  #print(sapply(p, function(x) x$type == "n"))
  #print(n.norm)
  if (n.norm > 1) {
    ## if no mean exists (e.g. exponential), set it to 1
    intermed_comp <- p[[which.min(sapply(p, function(x) ifelse(x$type == "e", 1, x$L)))]]
    ## except "type" that is not numeric
    if (as.vector) {
      intermed_comp <- unlist(intermed_comp[-1], recursive = FALSE)
    }
  } else {
    intermed_comp <- NA
  }
  intermed_comp
}

#' @rdname mx_parmlist_sort
#'
#' @export
#'
mx_wildtype <- function(p, as.vector = TRUE) {
  ## main normal component with largest L

  if(isClass("mxObj", p)) p <- coef(p)

  ## if no mean exists (e.g. exponential), set it to zero
  main_comp <- p[[which.max(sapply(p, function(x) ifelse(x$type == "e", 0, x$L)))]]
  ## except "type" that is not numeric
  if (as.vector) main_comp <- unlist(main_comp[-1], recursive = FALSE)
  main_comp
}
