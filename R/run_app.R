# File: myshinyapp/R/run_app.R

#' Launch the Shiny App included in this package
#'
#' @description
#' Starts the Shiny application that’s stored in `inst/app/` inside this package.
#' @export
run_app <- function() {
  # Find the folder “app” inside the installed package
  app_dir <- system.file("app", package = "myshinyapp")
  if (app_dir == "C:/Users/alioubadara.samake/OneDrive - World Food Programme/Documents/myshinyapp/inst/app") {
    stop("Could not find the Shiny app directory. Did you install the ‘myshinyapp’ package correctly?")
  }
  shiny::runApp(app_dir, display.mode = "normal")
}

