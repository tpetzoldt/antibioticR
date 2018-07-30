#' Diagnostics of Mixture Objects Fitted with 'mle2'
#'
#' Return diagnostics after on a mixture distribution object fittted with 'mx_metafit'
#'
#' @param obj object of class \code{mxMle}
#' @param breaks class boundaries of the data
#' @param counts frequency of observations
#' @param ... reserved for future extensions
#'
#' @return list of diagnostic measures
#'
#' @exportMethod diagnostics
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
#' diagnostics(ret)   # additional diagnostics
#'
#' @rdname mx-diagnostics
#'
setGeneric("diagnostics", function(obj, ...) standardGeneric("diagnostics"))

#' @rdname mx-diagnostics
#'
setMethod("diagnostics", "mxMle",
          function(obj, breaks=NULL, counts=NULL, ...) {

            saved_data <- get_data(obj)

            if (length(saved_data) > 0) {
              breaks <- saved_data$breaks
              counts <- saved_data$counts
            }

            if(is.null(breaks) | is.null(counts)) stop("data missing")

            qual <- mx_compare(coef(obj), breaks, counts)

            ret <- list(
              conv = obj@fit@details$convergence,
              r2_var = qual$r2_var,
              r2_cor = qual$r2_cor,
              EF = qual$EF,
              logLik = logLik(obj),
              AIC =AIC(obj),
              N       = sum(counts),
              Nclasses = length((breaks[-1])[counts > 0])
            )

            ## todo: print something
            msg <- switch (paste0("case", ret$conv),
                           case0 = "Successful completion of optim",
                           case1 = "The iteration limit 'maxit' had been reached",
                           case10 = "Degeneracy of the Nelder-Mead simplex",
                           case51 = "L-BFGS-B returned with a warning",
                           case52 = "L-BFGS-B returned with an error"
            )


            cat("\nConvergence = ", ret$conv, ": ", msg, "\n")
            if (ret$conv > 50) cat(obj@fit@details$message, "\n") # thpe-todo: check if correct slot
            cat("\n")
            cat("Agreement of (cumulative) distribution to data:\n\n")
            cat("rsquared (explained variance):     ", ret$r2_var, "\n")
            cat("rsquared (cor(observed,predicted)):", ret$r2_cor, "\n")
            cat("Nash Sutcliffe Efficiency:         ", ret$EF, "\n")
            cat("Log Likelihood:                    ", ret$logLik, "\n")
            cat("AIC:                               ", ret$AIC, "\n")
            cat("Total number of counts:            ", ret$N, "\n")
            cat("Number of non-empty classes:       ", ret$Nclasses, "\n\n")

            ## return results
            invisible(ret)
          }
)
