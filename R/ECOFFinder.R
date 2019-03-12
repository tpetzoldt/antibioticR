#' Launch antibioticR App ECOFFinder
#'
#' Launch a shiny app to estimate MIC concentrations interactively.
#'
#' @param ... optional parameters passed to the app (not yet implemented)
#'
#' @details The app is still experimental.
#'
#' @return This function has no return value.
#'
#' @seealso \code{\link{ecoffinder_nls}} for a command line version of
#'   this function.
#'
#' @examples
#'
#' \dontrun{
#' ECOFFinder()
#' }
#'
#' @export
#'
ECOFFinder <- function(...) {
  shiny::runApp(appDir = system.file("ECOFFinder", package = "antibioticR"), ...)
}
