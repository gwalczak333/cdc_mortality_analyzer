# run-all.R — Build the site and/or launch the Shiny app
#
# Usage:
#   Rscript run-all.R app
#   Rscript run-all.R site
#   Rscript run-all.R app site
#   # If no args are provided, defaults to app.

args <- tolower(commandArgs(trailingOnly = TRUE))

run_app <- length(args) == 0 || "app" %in% args
run_site <- "site" %in% args || "quarto" %in% args || "render" %in% args

if (run_site) {
  if (!requireNamespace("quarto", quietly = TRUE)) {
    stop("Package 'quarto' is required to render the site. Install it with install.packages('quarto').")
  }
  quarto::quarto_render()
}

if (run_app) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required to run the app. Install it with install.packages('shiny').")
  }
  shiny::runApp()
}

if (!run_app && !run_site) {
  stop("Unknown argument(s). Use: app, site, or both.")
}
