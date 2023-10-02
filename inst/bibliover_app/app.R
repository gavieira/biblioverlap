library(shiny)
library(biblioverlap)
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
                 textInput("di", "DOI / unique identifier", value = 'DOI')
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
      tabsetPanel(
        id = "results",
        tabPanel("all_data",
                 tags$br(),
                 uiOutput('download_button'),
                 tags$br(),
                 DT::dataTableOutput('full_table')
        ),
        tabPanel("Summary",
                 #Could have both a table and a plot, and donwload boxes for both of them.
        ),
        tabPanel("Venn Diagram",
        ),
        tabPanel("UpSet plot",
        )

        )


      #tableOutput('full_table'),
      #tableOutput('internal_table'),
      #DT::dataTableOutput('internal_table'),
      #DT::dataTableOutput('full_table')

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

  read_input_files <- function(input_files, sep, quote) {

    df_list <- lapply(input_files, function(input_file) {
      read.csv(input_file,
               sep = sep,
               strip.white = TRUE,
               check.names = FALSE) })
    df <- do.call(rbind, df_list)

    # There's no need to remove duplicate rows here, as biblioverlap already does that in the removing_duplicates() function
    #df <- df[!duplicated(df), ]


    return( df )
  }


  # Function to retrieve information from all sets
  get_sets_list <- function() {
    all_sets <- list()
    for (i in 1:input$n_sets) {
      set_name <- input[[paste0("name", i)]]
      filepaths <- input[[paste0("files", i)]]$datapath
      sep <- input[[paste0("sep", i)]]
      quote <- input[[paste0("quote", i)]]

      # Read the CSV file and store it in a data frame
      df <- read_input_files(filepaths, sep, quote)

      # Assign the data frame to the named list
      all_sets[[set_name]] <- df
      }
    return(all_sets)
  }



  get_merged_db_list <- function(db_list) {
    df <- do.call(rbind, Map(cbind, db_list, SET_NAME = names(db_list))) #Joining all info in a single table, while also adding a new column (SET_NAME) with the name of the set that record comes from
    columns_to_front <- c("SET_NAME", "UUID") # Specifying the names of the columns to be moved to the front
    df <- df[c(columns_to_front, setdiff(names(df), columns_to_front))] # Rearrange columns

    return( df )
  }


  #get_merged_db_list <- function(db_list) {
  #  db_list <- lapply(db_list, function(df) as.data.frame(lapply(df, as.character))) #Converting all field types to character to avoid problems when merging the dataframes
  #  return( dplyr::bind_rows(db_list, .id = 'SET_NAME') )
  #}


  observeEvent(input$compute, {
  output$download_button <- renderUI({
    downloadButton("download_data", "Download Data") })
  # Retrieve information from all sets
  View(input$files1)
  View(input$files2)
  })

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
        dom = "Blrtip",
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


  output$download_data <- downloadHandler(
    filename = function() {
      'result_data.csv'
    },
    content = function(file) {
      write.csv(get_merged_db_list(calculate_results()$db_list), file, row.names = FALSE)
    }
  )

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
