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

    check_installation("purrr")

    packages <- c(
        "curl", "shiny", "shinyFeedback", "shinyWidgets", "DT", "bslib",
        "stringr", "formattable", "dplyr", "emo"
    )

    purrr::walk(packages, check_installation)

    if (!curl::has_internet())
        stop("Please make sure you have an internet connection.")

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
