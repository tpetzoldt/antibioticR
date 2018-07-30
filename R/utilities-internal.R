## === internal functions ===

## A trivial helper function that just multiplies the results of pnorm with fmax.
fnorm <- function(x, mean, sd, fmax) {
  pnorm(x, mean=mean, sd=sd) * fmax
}


## weighted sum function that avoids errors if x=+/-Inf, NA, NaN but w=0

wsum <- function(x, w) {
  sum(x[w !=0 ] * w[w != 0])
}

## "Top n" helper function, works also for larger vectors
## https://stackoverflow.com/questions/18450778/r-fastest-way-to-get-the-indices-of-the-max-n-elements-in-a-vector
##
whichpart <- function(x, n=1) {
  which(x >= -sort(-x, partial=n)[n])
}


## merge a data frame with itself according to two 'criteria' columns
## and optionally keep some extra columns. This can be useful for
## pairing rows for subsequent pairwise comparisons, differences, etc.
## thpe-todo: make this more general using one of R's join functions

merge2 <- function(data, by, split, criteria, cols, extra=NULL) {
  if (length(criteria) != 2) stop("length of 'criteria' must be exactly 2")
  x <- data[data[, split] == criteria[1], c(by, cols)]
  y <- data[data[, split] == criteria[2], c(by, cols, extra)]
  ret <- merge(x, y, by=by, suffixes=criteria)
  return(ret)
}


# ## thpe-todo: internal helper, redundant see mxObj-coerce, left here for debugging
# parmlist2vec <- function(p) {
#
#   if (is(p, "mxObj"))   p <- p@coef #thpe-todo: coef(p) does not work yet here
#   p <- mx_parmlist_sort(p) ##
#
#   ## remove "type" element (cannnot be converted to numeric)
#   for (i in 1:length(p)) p[[i]]$type <- NULL
#
#   for (i in 1:length(p)) {
#     nm <- names(p[[i]])
#     nm2 <- paste0(nm, i)
#     inm <-  which(nm != "type")
#     nm[inm] <- nm2[inm]
#     names(p[[i]]) <- nm
#   }
#   ## remove names at top level (keeping 2nd level)
#   names(p) <- NULL
#   unlist(p)
# }
