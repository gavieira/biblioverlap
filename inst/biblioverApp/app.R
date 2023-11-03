library(shiny)
if (!(require(DT, quietly=TRUE))){install.packages("DT")};require(DT, quietly =TRUE)
library(biblioverlap)

# Load the UI and server components
source('ui.R')
source('server.R')

# Run the Shiny app
shinyApp(ui = ui, server = server)
