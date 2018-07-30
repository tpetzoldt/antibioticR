#' Quantiles and Results of Mixture Objects Fitted with 'mle2'
#'
#' @param obj object of class \code{mxMle}
#' @param breaks class boundaries of the data
#' @param counts frequency of observations
#' @param ... reserved for future extensions
#'
#' @return list of results
#'
#' @examples
#'
#' breaks <- 0:28
#' counts <- c(36, 0, 2, 3, 4, 8, 9, 14, 10, 9, 3, 1, 1, 2,
#'             4, 8, 20, 45, 40, 54, 41, 22, 8, 3, 3, 0, 0,0)
#'
#' observations <- unbin(breaks[-1], counts) # upper class boundaries
#'
#' (comp <- mx_guess_components(observations, bw=2/3, mincut=0.9))
#' obj <- mxObj(comp, left="e")
#' ret <- mx_metafit(breaks, counts, obj)
#'
#' summary(ret)       # general summary
#' cov2cor(vcov(ret)) # correlation matrix of parameters
#' results(ret)   # additional diagnostics
#'
#' @rdname mx_results
#'
setGeneric("results", function(obj, ...) standardGeneric("results"))

#' @rdname mx_results
#' @exportMethod results
#'
setMethod("results", "mxMle",
          function(obj, breaks=NULL, counts=NULL, ...) {

            saved_data <- get_data(obj)

            if (length(saved_data) > 0) {
              breaks <- saved_data$breaks
              counts <- saved_data$counts
            }

            if(is.null(breaks) | is.null(counts)) stop("data missing")

            qual <- mx_compare(coef(obj), breaks, counts)

            ## --- prepare list of parameters in suitable format

            pL <- as.list(obj)
            ## bring parameters within a component in pre-defined order
            pL <- mx_parmlist_sort(pL)

            ## --- identify main component of mixture (wild type)

            ## a) the normal component with largest mean
            ## if no mean exists (e.g. exponential), set it to zero
            #which.max(sapply(pL, function(x) ifelse(is.null(x$mean), 0, x$mean)))

            ## b) the normal component with largest L
            ## if no mean exists (e.g. exponential), set it to zero
            main_comp <- pL[[which.max(sapply(pL, function(x) ifelse(x$type == "e", 0, x$L)))]]

            ## remove "type" that is not numeric
            main_comp <- unlist(main_comp[-1])

            coef <-  as.vector(obj)

            quality <- mx_compare(coef(obj), breaks, counts)

            q01 <- qnorm(0.01, main_comp["mean"], main_comp["sd"])
            q05 <- qnorm(0.05, main_comp["mean"], main_comp["sd"])
            q95 <- qnorm(0.95, main_comp["mean"], main_comp["sd"])
            q99 <- qnorm(0.99, main_comp["mean"], main_comp["sd"])

            x <- unbin(breaks[-1], counts) # upper class boundaries

            h <- hist(x, breaks=breaks, plot=FALSE)

            L_q01 <- sum(h$counts[(h$breaks[-1]) > q01])/sum(h$counts)

            ## --- return results
            t(
              c(components  = length(pL),
                coef,
                q01         = q01,
                q05         = q05,
                L_q01       = L_q01,
                r2_var      = quality$r2_var,
                r2_cor      = quality$r2_cor,
                EF          = quality$EF
              ))
          }
)
