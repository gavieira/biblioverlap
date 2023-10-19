library(shiny)
library(biblioverlap)
#options(shiny.maxRequestSize = 100 * 1024^2)

# Load the UI and server components
source('ui.R')
source('server.R')

# Run the Shiny app
shinyApp(ui = ui, server = server)
