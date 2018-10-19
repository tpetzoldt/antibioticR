#
#' Sort and Select Distribution Components
#'
#' @param p mixture distribution object or parameter list
#' @param N number of components
#' @param item parameter name used for sorting and selection (e.g. 'L', 'mean', 'sd')
#' @param type kind of subset
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

#' @rdname mx_parmlist_sort
#'
#' @export
#'
mx_select <- function(obj, type=c("resistant", "wildtype", "intermediate",
                                  "hypersensitive", "nonresistant")) {

  type <- match.arg(type)

  if(isClass("mxObj", obj)) p <- coef(obj)

  if (type=="wildtype") {
    ## main normal component with largest L
    ## if no mean exists (e.g. exponential), set it to zero
    ndx <- which.max(sapply(p, function(x) ifelse(x$type == "e", 0, x$L)))

  } else if (type == "resistant") {
    ndx <- which(sapply(p, function(x) x$type == "e"))

  } else if (type=="nonresistant") {
    ndx <- which(sapply(p, function(x) x$type != "e"))

  } else if (type=="hypersensitive") {
    wt <- mx_select(obj, "wildtype")
    mean_wt <- p[[wt]]$mean

    ndx <- which(sapply(p, function(x) {
      mean_x <- ifelse(x$type == "e", 0, x$mean) # set mean of exponential to zero
      mean_x > mean_wt}
    ))

  } else if (type=="intermediate") {
    wt <- mx_select(obj, "wildtype")
    mean_wt <- p[[wt]]$mean
    ndx <- which(sapply(p, function(x) {
      mean_x <- ifelse(x$type == "e", 0, x$mean)
      (mean_x > 0) & (mean_x < mean_wt)}
    ))
  }

  if (length(ndx) == 0) ndx <- NULL

  ndx
}

#' @rdname mx_parmlist_sort
#'
#' @export
#'
mx_subset <- function(p, type=c("resistant", "wildtype", "intermediate",
                                "hypersensitive", "nonresistant"), as.vector = TRUE) {

  ndx <- mx_select(p, type=type)

  if(isClass("mxObj", p)) p <- coef(p)

  if (is.null(ndx)) {
    component <- NULL
  } else {
    component <- p[ndx]
    ## strip list node if only one component
    if (length(component) == 1) {
      nested_list <- FALSE
      component <- p[[ndx]]
    } else {
      nested_list <- TRUE
    }
    if (as.vector) {
      # strip non-numeric type element from list
      if (nested_list) {
        tmp <- lapply(component, function(x) {
          x$type <- NULL
          x
        })
      } else {
        tmp <- component[-1] # except "type" element
      }
      component <- unlist(tmp)
      ## if > 1 component, substitute n1.sd, n1.mean with sd1, mean2, ...
      names(component) <- sub("^([a-z])([0-9]+)([.])(.+)", "\\4\\2", names(component))
    }
  }
  component
}


