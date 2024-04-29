#'@export
launchApp <- function() {
  if (!require(shiny)) {
    install.packages("shiny")
  }

  # Load the Shiny package
  library(shiny)

  appDir <- system.file("shinyapp", package = "statspackage")

  # Run the Shiny app
  if (nzchar(appDir)) {
    runApp(appDir)
  } else {
    message("Shiny app directory not found in the package")
  }
}
