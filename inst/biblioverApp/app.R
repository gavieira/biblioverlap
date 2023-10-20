library(shiny)
library(biblioverlap)

options(
  shiny.reactlog = TRUE,  # Enable or disable the Reactlog (optional)
  shiny.width = reactive(input$width),     # Set the default width for renderPlot
  shiny.height = reactive(input$height),    # Set the default height for renderPlot
  shiny.res = 100        # Set the default resolution (dpi) for renderPlot
)


# Load the UI and server components
source('ui.R')
source('server.R')

# Run the Shiny app
shinyApp(ui = ui, server = server)
