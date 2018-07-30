#' Coercion Methods for 'mxObj' Univariate Mixture Objects
#'
#' Conversion methods of
#'   univariate mixture distributions, composed of exponential, normal and gamma
#'   distributions.
#'
#' @param mxObj an object of class \code{'mxObj'}
#' @param from an object that is to be converted to another type
#' @param x an object that is to be converted to another type
#' @param ... reserved for future extension
#'
#' @name coerce
#'
#' @rdname mxObj-coerce
#'
setAs("mxObj", "list",
      function(from) {
        L <- from@coef
        ## todo: add names alredy by the constructor ?
        names(L) <- paste0(sapply(L, function(x) x$type), 1:length(L))
        L
      }
)


## thpe: how to document this? @name "as" ???
## thpe-todo: join this with potentially redundant internal function parmlist2vec
#' @name coerce
#' @rdname mxObj-coerce
#'
setAs("data.frame", "mxObj",
      function(from) {

        ## convert **rows** to lists
        pL <- apply(from, 1, as.list)

        ## convert "numeric" elements back to numeric, but not type
        pL <- lapply(pL, function(L) {
          L[c("mean", "sd", "L")] <- as.numeric(L[c("mean", "sd", "L")])
          L
        })

        ## convert parameters of exponential component
        if (pL[[1]]$type =="e") {
          ## for the exponential: rate = 1/mean = 1/sd
          pL[[1]]$rate <- 1/pL[[1]]$sd
          ## drop redundant parameters
          pL[[1]]$mean <- pL[[1]]$sd <- NULL
        }
        new("mxObj", coef=pL)
      }
)

## thpe-todo: join this with potentially redundant internal function parmlist2vec
#' @name coerce
#' @rdname mxObj-coerce
#'
setAs("mxObj", "vector",
      function(from) {

        p <- mx_parmlist_sort(coef(from)) # thpe-check
        #p <- mx_parmlist_sort(from)

        ## remove "type" element (cannnot be converted to numeric)
        for (i in 1:length(p)) p[[i]]$type <- NULL

        for (i in 1:length(p)) {
          nm <- names(p[[i]])
          nm2 <- paste0(nm, i)
          inm <-  which(nm != "type")
          nm[inm] <- nm2[inm]
          names(p[[i]]) <- nm
        }
        ## remove names at top level (keeping 2nd level)
        names(p) <- NULL
        unlist(p)
      }
)


## heuristic, works for exponential, normal and gamma distributions
#' @name coerce
#' @rdname mxObj-coerce
#'
setAs("vector", "mxObj",
      function(from) {

        nm <- names(from)

        ## identify number of components from number postfix of parameter names
        N <- max(as.numeric(gsub("[a-z,A-Z]+", "", nm)))
        plist <- list()
        for (i in 1:N) {
          ndx <- grep(i, nm)
          tmp <- as.list(from[ndx])
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
        new("mxObj", coef=plist)
      }
)

## --- S3 method wrappers ------------------------------------------------------

#' @rdname mxObj-coerce
#'
setGeneric("as.mxObj", function(from, ...) standardGeneric("as.mxObj"))

#' @rdname mxObj-coerce
#'
setMethod("as.mxObj", "list",
          function(from, ...) as(from, "mxObj")
)

#' @rdname mxObj-coerce
#'
setMethod("as.mxObj", "vector",
          function(from, ...) as(from, "mxObj")
)


#' @rdname mxObj-coerce
#'
setMethod("as.list", "mxObj",
          function(x, ...) as(x, "list")
)


#' @rdname mxObj-coerce
#'
setMethod("as.vector", "mxObj",
          function(x) as(x, "vector")
)
