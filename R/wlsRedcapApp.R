#' Start Shiny App
#'
#' Wrapper function to start shiny app.
#'
#' @export
wlsRedcapApp <- function() {
  shiny::runApp(appDir = system.file("shiny", package = "wlsRedcapApp"))
}
