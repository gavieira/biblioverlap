library(shiny)
options(shiny.maxRequestSize = 100 * 1024^2)

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
                 textInput("di", "DOI", value = 'DOI')
        ),
        tabPanel("TI",
                 textInput('ti', 'Document title', value = 'Title')
        ),
        tabPanel("SO",
                 textInput('so', 'Document Source', value = 'Source Title')
        ),

        tabPanel("AU",
                 textInput('au', 'Author(s)', value = 'Author/s')
        ),

        tabPanel("PY",
                 textInput('py', 'Publication Year', 'Publication Year')
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
    mainPanel(
      #tableOutput('full_table'),
      #tableOutput('internal_table'),
      DT::dataTableOutput('internal_table'),
      DT::dataTableOutput('full_table')

    )
  )
)

# Define server function that does nothing
server <- function(input, output, session) {

  # Get list of columns
  get_columns_list <- function() {
    col_list <- list(
      DI = input$di,
      TI = input$ti,
      SO = input$so,
      AU = input$au,
      PY = input$py
    )
  }

  read_input_file <- function(input_file, sep, quote) {
    if (quote == "") {
      df <- read.csv(input_file,
                    sep = sep,
                    strip.white = TRUE,
                    check.names = FALSE) }
    else {
      df <- read.csv(input_file,
                    sep = sep,
                    quote = quote,
                    strip.white = TRUE,
                    check.names = FALSE) }

    return( df )
  }


  # Function to retrieve information from all sets
  get_sets_list <- function() {
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





  get_merged_db_list <- function(db_list) {
    db_list <- lapply(db_list, function(df) as.data.frame(lapply(df, as.character))) #Converting all field types to character to avoid problems when merging the dataframes
    return( dplyr::bind_rows(db_list, .id = 'SET_NAME') )
  }


  #observeEvent(input$compute, {
  #  # Retrieve information from all sets
  #  sets_info <- get_sets_list()
  #  View(sets_info)
  #  View(sets_info[[1]])
  #  View(sets_info[[2]])
  #  # Do something with sets_info, e.g., print it
  #  #View(input$set1)
  #  #View(input$set2)
  #  #View(input$name1)
  #  #View(input$name2)
  #  #View(input$sep1)
  #  #View(input$sep2)
  #  #View(input$quote1)
  #  #View(input$quote2)
  #})


 generateUI <- function(id) {
   tagList(
      div(
        id = paste0("uiElement", id),
        # Add your UI elements here
        HTML(paste0("Set", id, ':')),
        tabsetPanel(
          id = paste0("set", id),
          tabPanel("Name",
                   textInput(paste0("name", id), "Dataset name:", value = id)
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
                               selectize = FALSE,
                               selected = "," )
          ),
          tabPanel("Quote",
                   selectInput(paste0('quote', id), 'Quote type',
                               choices = c("None" = "",
                                           "Double Quote" = '"',
                                           "Single Quote" = "'"),
                               selectize=FALSE,
                               selected = NULL)
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

  calculate_results <- eventReactive(input$compute, {
    withProgress(message = 'Analyzing data...', {
    columns <- get_columns_list()
    db_list <- get_sets_list()
    tryCatch({
      results <-  biblioverlap::biblioverlap(db_list, matching_fields = columns)
    }, error = function(e)
      message(e)
      )
    return( results )
    })
  })

  read_datatable <- function(df) {
    return(DT::datatable(
      df,
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
            "} }" )
            )
          )
        )
      )
      )
  }



  output$full_table <- DT::renderDataTable({
    table_list <- calculate_results()$db_list
    table <- get_merged_db_list(table_list)

    return( read_datatable(table) )
  }, server = TRUE)

  output$internal_table <- DT::renderDataTable({
    table_list <- calculate_results()$internal_db_list
    table <- get_merged_db_list(table_list)
    #DT::datatable(table)
    return( read_datatable(table) )
  }, server = TRUE)

}

# Create the app object
shinyApp(ui = ui, server = server)
