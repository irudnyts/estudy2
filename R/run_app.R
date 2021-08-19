#' @export
run_app <- function() {

    if (!requireNamespace("shiny", quietly = TRUE)) {
        stop(
            paste(
                "Package {shiny} is needed for this function to work.",
                "Please install it."
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
