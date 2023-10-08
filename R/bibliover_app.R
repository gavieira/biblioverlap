#' Title
#'
#' @param port - port of the application
#' @param max_upload_size - max upload size of documents (in MB) - Default 100
#' @param launch.browser - launch on browser - Default = TRUE
#'
#' @return opens a instance of the biblioverlap UI
#' @export
#'
# @examples
biblioverApp <- function(port = NULL, max_upload_size = 1000, launch.browser = TRUE) {
  options(shiny.maxRequestSize = max_upload_size * 1024 * 1024)
  shiny::shinyAppDir(
    system.file("biblioverApp", package = "biblioverlap"),
    options = list(port = port, launch.browser = launch.browser)
  )
}
