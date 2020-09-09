#' @title Classes of package \pkg{antibioticR} ...
#'
#' @description The classes define ...
#'
#' @details The \code{abr_ecoffinder}: class represents a distribution component
#' fitted with the ECOFFinder method.
#'
#' @name abr_baseclass
#'
setClass("abr_baseclass",
         representation(
           data = "data.frame",
           fit = "nls",     # todo: is "nls", should extend "list"
           coef = "numeric"
         )
)


#' @rdname abr_baseclass
#' @exportClass abr_ecoffinder
#'
setClass("abr_ecoffinder",
         representation(
           #data = "data.frame",
           concentrations = "numeric", # concentration subsets
           startpar = "numeric",
           i_best = "numeric",
           #fit = "nls",     # todo: is "nls", should extend "list"
           models = "list",
           #coeff = "numeric",
           r2 = "numeric", # experimental
           quantiles = "numeric",
           ecoff = "numeric",
           log2 = "logical"     # whether ecoffs are 2^ecoff transformed or not
         ),
         contains = "abr_baseclass"
)


