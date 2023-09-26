bibliover_app <- function(host = "127.0.0.1", port = NULL,
                        launch.browser = TRUE, maxUploadSize = 1000
){

  shiny::shinyOptions(maxUploadSize = maxUploadSize)

  #shiny::runApp(system.file("bibliover_app",package="biblioverlap"))
  shiny::runApp(system.file("bibliover_app",package="biblioverlap"),launch.browser = launch.browser, port = port, host = getOption("shiny.host", host))
}
