library(shiny)
library(biblioverlap)

# Load the UI and server components
source('ui.R')
source('server.R')

# Run the Shiny app
shinyApp(ui = ui, server = server)
