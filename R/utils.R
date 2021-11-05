#' Check whether the package is installed
#'
#' \code{check_installation} checks whether the package is installed, and if
#' not, aborts the execution with the clear message.
#'
#' The function uses the \code{stop()} function to abort the execution. The
#' function is not vectorized.
#'
#' @param package_name a lenght-one character vector that contains the name of
#' the package to check
#'
#' @example
#' check_installation("shiny")
#'
#' @noRd
check_installation <- function(package_name) {

    if (!requireNamespace(package_name, quietly = TRUE)) {
        stop(
            paste0(
                "Package {", package_name, "} is needed for this function to ",
                "work. Please install it."),
            call. = FALSE
        )
    }

}
