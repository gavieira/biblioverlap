library(shiny)

# Define UI with fluid rows and columns
ui <- fluidPage(
  titlePanel('Biblioverlap'),
  tags$style(HTML(".compute_button { background-color: green; color: white; }")),
  sidebarLayout(
    sidebarPanel(

      tags$b('Column names'),

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

      numericInput('n_threads', 'Number of threads',
                   min = 1,
                   max = parallel::detectCores(),
                   value = 1
                   ),


      tags$hr(),

      numericInput('n_sets', "Number of bibliographical datasets",
                   min = 2, max = 7,
                   value = 2),
      uiOutput("dynamicUI"),

      actionButton('compute', "Compute", width = '100%', class = 'compute_button')
    ),
    mainPanel()
  )
)

# Define server function that does nothing
server <- function(input, output, session) {

  # Get list of
  column_list <- function() {

  }

  read_input_file <- function(input_file, sep, quote) {
    df <- read.csv(input_file,
                   sep = sep,
                   quote = quote,
                   strip.white = TRUE)

    return( df )
  }


  # Function to retrieve information from all sets
  retrieveInfoFromSets <- function() {
    all_sets <- list()
    for (i in 1:input$n_sets) {
      set_name <- input[[paste0("name", i)]]
      filepath <- input[[paste0("files", i)]]$datapath
      sep <- input[[paste0("sep", i)]]
      quote <- input[[paste0("quote", i)]]

      # Read the CSV file and store it in a data frame
      df <- read_input_file(filepath, sep, quote)

      # Assign the data frame to the named list
      all_sets[[set_name]] <- df
      }
    return(all_sets)
  }

  observeEvent(input$compute, {
    # Retrieve information from all sets
    sets_info <- retrieveInfoFromSets()
    View(sets_info)
    View(sets_info[[1]])
    View(sets_info[[2]])
    # Do something with sets_info, e.g., print it
    #View(input$set1)
    #View(input$set2)
    #View(input$name1)
    #View(input$name2)
    #View(input$sep1)
    #View(input$sep2)
    #View(input$quote1)
    #View(input$quote2)
  })


  colnames <- reactiveValues(
    DI = NULL,
    TI = NULL,
    SO = NULL,
    AU = NULL,
    PY = NULL
  )

 n_threads <- reactiveVal(NULL)
 n_sets <- reactiveVal(NULL)





 generateUI <- function(id) {
   tagList(
      div(
        id = paste0("uiElement", id),
        # Add your UI elements here
        HTML(paste0("Set", id, ':')),
        tabsetPanel(
          id = paste0("set", id),
          tabPanel("Name",
                   textInput(paste0("name", id), "Dataset name:")
          ),
          tabPanel("Files",
                   fileInput(paste0("files", id), "Upload files:", multiple = TRUE,
                             accept = c("text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv") )
          ),
          tabPanel("Sep",
                   selectInput(paste0('sep', id), 'Separator',
                               choices = c(Comma = ",",
                                           Semicolon = ";",
                                           Tab = "\t"),
                               selected = "," )
          ),
          tabPanel("Quote",
                   selectInput(paste0('quote', id), 'Quote type',
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
    uiList <- lapply(1:input$n_sets, function(i) {
      generateUI(i)
    })
    tagList(uiList)
  })

  eventReactive(input$compute, {
    matching_fields <- list(
      DI = input$di,
      TI = input$TI,
      AU = input$AU,
      SO = input$SO)
    sets = list()


  })





}

# Create the app object
shinyApp(ui = ui, server = server)
