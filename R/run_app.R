# File: wfp-data-quality-app/R/run_app.R

#' Launch the Shiny App included in this package
#'
#' @description
#' Starts the Shiny application that's stored in `inst/app/` inside this package.
#' @export
run_app <- function() {
  # Find the folder "app" inside the installed package
  app_dir <- system.file("app", package = "wfp.data.quality.app")
  if (app_dir == "") {
    stop("Could not find the Shiny app directory. Did you install the 'wfp.data.quality.app' package correctly?")
  }
  shiny::runApp(app_dir, display.mode = "normal")
}


