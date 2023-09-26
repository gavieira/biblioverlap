library(shiny)

ui <- fluidPage(
  actionButton("addButton", "+ Add UI"),
  actionButton("removeButton", "- Remove UI"),
  uiOutput("dynamicUI")
)

server <- function(input, output, session) {
  # Initialize a counter for the number of added UI elements, starting at 1
  counter <- reactiveVal(1)

  # Define a function to generate the dynamic UI
  generateUI <- function(id) {
    tagList(
      div(
        id = paste0("uiElement", id),
        h4(paste("Dynamic UI Element", id)),
        # Add your UI elements here
        textInput(inputId = paste0("text", id), label = "Text Input")
      )
    )
  }

  # Render the dynamic UI
  output$dynamicUI <- renderUI({
    uiList <- lapply(1:counter(), function(i) {
      generateUI(i)
    })
    tagList(uiList)
  })

  # Add UI element when the "+" button is clicked
  observeEvent(input$addButton, {
    counter(counter() + 1)
    insertUI(
      selector = "#dynamicUI",
      ui = generateUI(counter())
    )
  })

  # Remove UI element when the "-" button is clicked
  observeEvent(input$removeButton, {
    if (counter() > 0) {
      counter(counter() - 1)
      removeUI(
        selector = paste0("#uiElement", counter() + 1)
      )
    }
  })
}

shinyApp(ui, server)
