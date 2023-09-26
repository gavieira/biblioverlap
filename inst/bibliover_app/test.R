library(shiny)
library(DT)

# Define UI for data upload app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Uploading Files"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Select files ----
      fileInput("files", "Choose CSV Files",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),

      # Horizontal line ----
      tags$hr(),

      # Input: Checkbox if files have header ----
      checkboxInput("header", "Header", TRUE),

      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),

      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),

      # Horizontal line ----
      tags$hr(),

      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Data file ----
      uiOutput('downloadButtonContainer'),
      tags$br(),
      DT::dataTableOutput("contents")

    )

  )
)

# Define server logic to read selected files ----
server <- function(input, output) {
  options(shiny.maxRequestSize = 300 * 1024^2) # Set maximum request size to 300 MB

  output$downloadButtonContainer <- renderUI({
    if (is.null(input$files)) {
      #div()
    } else {
      downloadButton("downloadData", "Download Data")
    }
  })

  observe({
    #View(input$files)
    #cat(is.null(input$files))
    #cat(is.null(input$files))
    #cat(output$fileUploaded)
  })


  check_value <- renderPrint({show_value()})

  combined_data <- eventReactive(input$files, {
    req(input$files)

    df <- read.csv(input$files$datapath, header = input$header, sep = input$sep, quote = input$quote)
    return(df)
  })

  output$contents <- DT::renderDataTable({
    return(DT::datatable(
      combined_data(),
      extensions = c("Buttons", "ColReorder"),
      filter = 'top',
      selection = 'none',
      options = list(
        dom = "Blrptip",
        buttons = c("colvis"),
        colReorder = TRUE,
        columnDefs = list(
          list(targets = "_all", render = htmlwidgets::JS(
            "function(data, type, row, meta) {",
            "if (data === null || data === undefined) {",
            "return '';",
            "} else if (type === 'display' && data.length > 30) {",
            "return data.substr(0, 30) + '...';",  # Adjust the length as needed
            "} else {",
            "return data;",
            "} }"
          ))
        )
      )
    ))
  }, server = TRUE)

  length_files <- reactiveVal(0)

  observeEvent(input$files, {
    new_value <- length(input$files)
    length_files(new_value)
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("all_data", ".tsv")
    },
    content = function(file) {
      write.csv(combined_data(), file)
    }
  )

  # Track whether a file has been uploaded
  output$fileUploaded <- reactive({
    return(is.null(input$files))
  })

  reactiveShowText <- reactive({
    input$showText
  })

}

# Create Shiny app ----
shinyApp(ui, server)
