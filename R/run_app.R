#' Run Shiny demo app
#'
#' The function \code{run_app()} launches a Shiny app, which is a GUI wrapper
#' of crippled version of \code{{estudy2}}. This app is developed exclusively
#' for demonstration purposes.
#'
#' The app is run locally.
#'
#' @export
run_app <- function() {

    if (!requireNamespace("shiny", quietly = TRUE) ||
        !requireNamespace("shinyWidgets", quietly = TRUE) ||
        !requireNamespace("bslib", quietly = TRUE)) {
        stop(
            paste(
                "Packages {shiny}, {shinyWidgets}, and {bslib} are needed for",
                "this function to work. Please install it."
            ),
            call. = FALSE
        )
    }

    app_dir <- system.file("app", package = "estudy2")

    if (app_dir == "") {
        stop(
            paste(
                "Cannot find the app directory.",
                "Please report at https://github.com/irudnyts/estudy2/issues"
            ),
            call. = FALSE
        )
    }

    shiny::runApp(app_dir, display.mode = "normal")
}
