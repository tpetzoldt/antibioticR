
#' @title Class of Univariate Mixture Distribution Components and its Parameters
#'
#' @description The classes define ...
#'
#' @slot coef coefficients of the mixture distribution, specified by a
#'   one-character ID of the distribution and the appropriate parameters
#'
#' @rdname mxObj-class
#' @exportClass mxObj
#'
setClass("mxObj",
         representation(
           coef = "list"
         )
)


#' @title Mixture Distribution Object Containing an 'mle2' fit as Subobject
#'
#' @description The classes define ...

#' @slot coef list with the coefficients of the mixture distribution
#' @slot fit complete results of the \code{mle2}-fit
#' @slot data optionally, data used for the fit
#'
#' @rdname mxMle-class
#'
#' @exportClass mxMle
#'
setClass("mxMle",
         representation(
           fit   = "ANY",  # should be "mle2"
           data = "list"   # optional
         ),
         contains = "mxObj"
)
