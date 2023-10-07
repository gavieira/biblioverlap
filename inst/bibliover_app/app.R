library(shiny)
library(biblioverlap)
options(shiny.maxRequestSize = 100 * 1024^2)

ui <- fluidPage(
  titlePanel('Biblioverlap'),
  tags$style(HTML(".custom_button { background-color: green; color: white; }")),
  sidebarLayout(
    sidebarPanel( width = 3,
      tabsetPanel(id = 'sidebars',
                  tabPanel('Biblioverlap',
                           tags$br(),

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
      tags$strong('Score matching'),
      tags$br(),

      actionButton("change_score_params", "Change parameters", class = 'custom_button'),
      tags$br(),
      conditionalPanel(
        condition = "input.change_score_params % 2 == 1",
        tags$br(),
        fluidRow(
          column(6, numericInput('ti_penalty', 'ti_penalty',
                                 min = 0, step = 0.1,
                                 value = 0.1)),
          column(6, numericInput('ti_max', 'ti_max',
                                 min = 0, step = 0.1,
                                 value = 0.6))
          ),
        fluidRow(
          column(6, numericInput('so_penalty', 'so_penalty',
                                 min = 0, step = 0.1,
                                 value = 0.1)),
          column(6, numericInput('so_max', 'so_max',
                                 min = 0, step = 0.1,
                                 value = 0.3))
          ),
        fluidRow(
          column(6, numericInput('au_penalty', 'au_penalty',
                                 min = 0, step = 0.1,
                                 value = 0.1)),
          column(6, numericInput('au_max', 'au_max',
                                 min = 0, step = 0.1,
                                 value = 0.3))
          ),
        fluidRow(
          column(6, numericInput('py_max', 'py_max',
                                 min = 0, step = 0.1,
                                 value = 0.3)),
          column(6, numericInput('score_cutoff', 'score_cutoff',
                                 min = 0, step = 0.1,
                                 value = 1))
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

      actionButton('compute', "Compute", width = '100%', class = 'custom_button')
    ),
    tabPanel('Merge Files',
             HTML('<br>Biblioverlap accepts a single csv file for each dataset. However, there are cases when a query has to be split between multiple files. <br> <br>
                  In this page, the user can upload multiple csv files (from the same bibliographical database) and download all records merged into a single file. <br> <br>'),
             tabsetPanel(id = 'merging_user_input',
                         tabPanel('Files',
             fileInput('unmerged_files',  'Upload files', multiple = TRUE,
                       accept = c("text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv") ) ),
             tabPanel('Sep',
             selectInput('unmerged_sep', 'Separator',
                                   choices = c(Comma = ",",
                                               Semicolon = ";",
                                               Tab = "\t"),
                                   selectize = FALSE,
                                   selected = "," ) ),
             ),
             actionButton('merge_button', "Merge Files", width = '100%', class = 'custom_button')

    )
  )
    ),
    mainPanel( width = 9,
      conditionalPanel(
        condition = "output.calculation_done",
      tabsetPanel(
        id = "results",
        tabPanel("Data",
                 tags$br(),
                 downloadButton("download_data", "Download Data", class = 'custom_button'),
                 tags$br(),
                 DT::dataTableOutput('full_table')
        ),
        tabPanel("Summary",
                 #Could have both a table and a plot, and donwload boxes for both of them.
                 downloadButton("download_summary_table", "Download Summary Table", class = 'custom_button'),
                 downloadButton("download_summary_plot", "Download Summary Plot", class = 'custom_button'),
                 tableOutput('summary_table'),
                 plotOutput('summary_plot', width = '100%')
                 #plotOutput('summary_plot', width = "1920px", height = "1080px")
        ),
        tabPanel("Venn Diagram",
                 plotOutput('venn', width = '100%')
        ),
        tabPanel("UpSet Plot",
                 tags$br(),
                 actionButton("modify_upset", "Modify plot", class = 'custom_button'),
                 conditionalPanel(
                   condition = "input.modify_upset % 2 == 1",
                   fluidRow(
                   column(2, numericInput('nintersects', 'Intersects',
                                min = 3, max = 150,
                                value = 30)
                          ),
                   column(2, selectInput('scale', 'Scale',
                               choices = c(Identity = "identity",
                                           log10 = "log10",
                                           log2 = "log2"),
                               selectize = FALSE,
                               selected = "identity" )
                          ),
                   column(2, numericInput('text_size', 'Text size',
                                min = 1, max = 3, step = 0.1,
                                value = 1.5)
                          ),
                   column(2, numericInput('mb.ratio', 'Barplot size',
                                min = 0.1 , max = 0.9, step = 0.05,
                                value = 0.7)
                          ),
                   column(2, selectInput('show.numbers', 'Barplot numbers',
                               choices = c(Show = "yes",
                                           Hide = "no"),
                               selectize = FALSE,
                               selected = "yes" )
                          )
                   )
                   ),
                 tags$br(),
                 plotOutput('upset', width = '100%')
                 )
        )

    )
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

  # Function to read multiple input_files per dataset
  read_input_files <- function(input_files, sep, quote) {

    df_list <- lapply(input_files, function(input_file) {
      read.csv(input_file,
               sep = sep,
               strip.white = TRUE,
               check.names = FALSE) })
    df <- do.call(rbind, df_list)
    #df <- df[!duplicated(df), ]   # There's no need to remove duplicate rows here, as biblioverlap already does that in the removing_duplicates() function

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


#  count_modify_upset <- reactiveVal(0)
#
#  observeEvent(input$modify_upset, {
#    count_modify_upset(count_modify_upset() + 1)
#  })
#
#  count_change_score_params <- reactiveVal(0)
#
#  observeEvent(input$change_score_params, {
#    count_change_score_params(count_change_score_params() + 1)
#  })

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

 observe(calculate_results())

  # Render the dynamic UI
  output$dynamicUI <- renderUI({
    uiList <- lapply(1:input$n_sets, function(i) {
      generateUI(i)
    })
    tagList(uiList)
  })


  calculation_done <- reactiveVal(FALSE) #Creating a new reactiveVal to hide the results panel until submitting data for analysis

  calculate_results <- eventReactive(input$compute, {
    withProgress(message = 'Analyzing data...', {
    columns <- get_columns_list()
    db_list <- get_sets_list()
    tryCatch({
      results <-  biblioverlap::biblioverlap(db_list,
                                             matching_fields = columns,
                                             ti_penalty = input$ti_penalty, ti_max = input$ti_max,
                                             so_penalty = input$so_penalty, so_max = input$so_max,
                                             au_penalty = input$au_penalty, au_max = input$au_max,
                                             py_max = input$py_max, score_cutoff = input$score_cutoff)
    }, error = function(e)
      message(e)
      )
    calculation_done(TRUE) #Setting this to TRUE here shows the result panel only when the calculation has been finished
    return( results )
    })
  })

  output$calculation_done <- reactive({ calculation_done() } ) #conditionalPanel does not accept reactiveVals, so we need to create a new output value
  outputOptions(output, "calculation_done", suspendWhenHidden = FALSE) #Needed to use output value in conditionalPanel (https://github.com/rstudio/shiny/issues/1318)



  read_datatable <- function(df) {
    return(DT::datatable(
      df,
      extensions = c("Buttons", "ColReorder"),
      filter = 'top',
      selection = 'none',
      options = list(
        dom = "Blrtip",
        scrollX = TRUE,
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

  output$download_summary_table <- downloadHandler(
    filename = function() {
      'summary.csv'
    },
    content = function(file) {
      write.csv(calculate_results()$summary$df, file, row.names = FALSE)
    }
  )


  output$download_summary_plot <- downloadHandler(
    filename = function() {
      'summary.csv'
    },
    content = function(file) {
      write.csv(calculate_results()$summary$df, file, row.names = FALSE)
    }
  )

  output$full_table <- DT::renderDataTable({
    table_list <- calculate_results()$db_list
    table <- get_merged_db_list(table_list)

    return( read_datatable(table) )
  }, server = TRUE) ##Server is necessary because the db_list can be huge

  output$summary_table <- renderTable({
    summary_table <- calculate_results()$summary$df
    #DT::datatable(table)
    return( summary_table )
  }, width = '100%', striped = TRUE, bordered = TRUE, align = 'l')

  output$summary_plot <- renderPlot({
    summary_plot <- calculate_results()$summary$plot
    summary_plot <- summary_plot + ggplot2::update_geom_defaults("text", list(size = 5)) + ggplot2::theme(text=ggplot2::element_text(size=15))
    return( summary_plot )
  })

  output$venn <- renderPlot({
    venn <- biblioverlap::plot_venn(calculate_results()$db_list)
    return( venn )
  })

  output$upset <- renderPlot({
    db_list <- calculate_results()$db_list
    upset <- biblioverlap::plot_upset(db_list,
                                      nsets = length(db_list),
                                      nintersects = input$nintersects,
                                      scale.intersections	= input$scale,
                                      scale.sets = input$scale,
                                      text.scale = input$text_size,
                                      show.numbers = input$show.numbers,
                                      mb.ratio = c(input$mb.ratio, 1 - input$mb.ratio)
    )
    return( upset )
  })

}


# Create the app object
shinyApp(ui = ui, server = server)
