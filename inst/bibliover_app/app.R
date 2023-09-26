library(shiny)

# Define UI with fluid rows and columns
ui <- fluidPage(
  titlePanel('Biblioverlap'),
  tags$style(HTML(".compute_button { background-color: green; color: white; }")),
  sidebarLayout(
    sidebarPanel(

      HTML('Adding column names:'),

      tabsetPanel(
        id = "colnames",
        tabPanel("DI",
                 textInput("doi", "DOI")
        ),
        tabPanel("TI",
                 textInput('ti', 'Document title')
        ),
        tabPanel("SO",
                 textInput('so', 'Document Source')
        ),

        tabPanel("AU",
                 textInput('au', 'Author(s)')
        ),

        tabPanel("PY",
                 textInput('py', 'Publication Year')
        )
      ),

      tags$hr(),

      numericInput('n_threads', 'Number of Threads',
                   min = 1,
                   max = parallel::detectCores(),
                   value = 1
                   ),


      tags$hr(),

      HTML('Adding bibliographical datasets:'),
      br(),
      br(),

      uiOutput("dynamicUI"),
      actionButton("addButton", "+ Add set"),
      actionButton("removeButton", "- Remove set"),



      actionButton('compute', "Compute", width = '100%', class = 'compute_button')
    ),
    mainPanel()
  )
)

# Define server function that does nothing
server <- function(input, output, session) {

  # Initialize a counter for the number of added UI elements
  counter <- reactiveVal(2)

  # Define a function to generate the dynamic UI
  generateUI <- function(id) {
    tagList(
      div(
        id = paste0("uiElement", id),
        # Add your UI elements here
        HTML(paste0("Set", id, ':')),
        tabsetPanel(
          id = paste0("set", id),
          tabPanel("Name",
                   textInput("name1", "Dataset name:")
          ),
          tabPanel("Files",
                   fileInput("set1", "Upload files:", multiple = TRUE,
                             accept = c("text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv") )
          ),
          tabPanel("Sep",
                   selectInput('sep1', 'Separator',
                               choices = c(Comma = ",",
                                           Semicolon = ";",
                                           Tab = "\t"),
                               selected = "," )
          ),
          tabPanel("Quote",
                   selectInput('sep1', 'Quote type',
                               choices = c(None = "",
                                           "Double Quote" = '"',
                                           "Single Quote" = "'"),
                               selected = "" )
          )
        )

      )
    )
  }

  # Render the dynamic UI
  output$dynamicUI <- renderUI({
    uiList <- lapply(1:2, function(i) {
      generateUI(i)
    })
    tagList(uiList)
  })


  # Add UI element when the "+" button is clicked
  observeEvent(input$addButton, {
    if (counter() < 7) {
    counter(counter() + 1)
    insertUI(
      selector = "#dynamicUI",
      ui = generateUI(counter())
    )}
  })

  # Remove UI element when the "-" button is clicked
  observeEvent(input$removeButton, {
    if (counter() > 2) {
      counter(counter() - 1)
      removeUI(
        selector = paste0("#uiElement", counter() + 1)
      )
    }
  })

}

# Create the app object
shinyApp(ui = ui, server = server)
