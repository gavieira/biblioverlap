server <- function(input, output, session) {


  # Get list of columns from UI
  get_columns_list <- function() {
    col_list <- list(
      DI = input$di,
      TI = input$ti,
      SO = input$so,
      AU = input$au,
      PY = input$py
    )
  }


  #Read individual input files for bibliographical overlap
  read_input_file <- function(input_file, sep, quote) {
      df <- read.csv(input_file,
                     sep = sep,
                     quote = quote,
                     strip.white = TRUE,
                     check.names = FALSE) #Keep names exactly as they are in the original files
    return( df )
  }


  # Function to retrieve information from all sets
  get_sets_list <- function() {
    all_sets <- list()
    for (i in 1:input$n_sets) {
      #Reading info from input
      set_name <- input[[paste0("name", i)]]
      filepath <- input[[paste0("file", i)]]$datapath
      sep <- input[[paste0("sep", i)]]
      quote <- input[[paste0("quote", i)]]

      df <- read_input_file(filepath, sep, quote) # Read the CSV file and store it in a data frame
      all_sets[[set_name]] <- df # Assign the data frame to the named list
    }
    return(all_sets)
  }


 generate_dataset_input_fields <- function(id) {
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
          tabPanel("File",
                   fileInput(paste0("file", id), "Upload file (csv only):",
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
                                          "Double Quote" = "\"",
                                          "Single Quote" = "'"),
                              selectize=FALSE,
                              selected = "\"")
         )
        )

      )
    )
 }


  # Render the dynamic UI
  output$dataset_input_fields <- renderUI({
    uiList <- lapply(1:input$n_sets, function(i) {
      generate_dataset_input_fields(i)
    })
    tagList(uiList)
  })


  #Calculating results using the biblioverlap package when the 'submit' button is clicked
  calculate_results <- eventReactive(input$submit, {
    withProgress(message = 'Analyzing data...', {
    columns <- isolate(get_columns_list())
    db_list <- isolate(get_sets_list())
    tryCatch({
      results <-  biblioverlap::biblioverlap(db_list,
                                             matching_fields = columns,
                                             ti_penalty = input$ti_penalty, ti_max = input$ti_max,
                                             so_penalty = input$so_penalty, so_max = input$so_max,
                                             au_penalty = input$au_penalty, au_max = input$au_max,
                                             py_max = input$py_max, score_cutoff = input$score_cutoff)
    calculation_done(TRUE) #Setting this to TRUE here shows the result panel only when the calculation has been finished
    return( results )
    }, error = function(err)
      showNotification(paste0(err), type = 'err', duration = NULL)
      )
    })
  })


  observe(calculate_results()) #Needed to 'hide' MainPanel before calculating the results

  calculation_done <- reactiveVal(FALSE) #Creating a new reactiveVal to hide the results panel until submitting data for analysis

  output$calculation_done <- reactive({ calculation_done() } ) #conditionalPanel does not accept reactiveVals directly, so we need to create a new output value from the reactive value

  outputOptions(output, "calculation_done", suspendWhenHidden = FALSE) #Needed to use output value in conditionalPanel (https://github.com/rstudio/shiny/issues/1318)


  read_datatable <- function(df) {
    return(DT::datatable(
      df,
      #extensions = c("Buttons", "ColReorder"),
      filter = 'top',
      selection = 'none',
      options = list(
        #dom = "Blrtip",
        dom = "lrtip",
        scrollX = TRUE,
        lengthMenu = c(2, 10, 50, 100),
        #buttons = c("colvis"),
        #colReorder = TRUE,
        columnDefs = list(
          list(targets = "_all", render = htmlwidgets::JS( #The following JS code shows only the first characters of each cell, replacing the rest with '...'
            "function(data, type, row, meta) {",
            "if (data === null || data === undefined) {",
            "return '';",
            "} else if (type === 'display' && data.length > 30) {",
            "return data.substr(0, 30) + '...';",  # Adjust the length of the substring to be displayed
            "} else {",
            "return data;",
            "} }" )
            )
          )
        )
      )
      )
  }

  results_data_table <- reactive ({
    return ( biblioverlap::merge_results(calculate_results()$db_list, filter_distinct = input$filter_distinct) )
  }  )


  output$download_data <- downloadHandler(
    filename = function() {
      'result_data.csv'
    },
    content = function(file) {
      write.csv(results_data_table(), file, row.names = FALSE)
    }
  )


  output$download_summary_table <- downloadHandler(
    filename = function() {
      'summary.csv'
    },
    content = function(file) {
      write.csv(calculate_results()$summary, file, row.names = FALSE)
    }
  )


  output$full_table <- DT::renderDataTable({
    return( read_datatable(results_data_table()) )
  }, server = TRUE) ##Server is necessary because the db_list can be huge

  output$summary_table <- renderTable({
    summary_table <- calculate_results()$summary
    return( summary_table )
  }, width = '100%', striped = TRUE, bordered = TRUE, align = 'l')


  #### Generating plots (reactive functions)

  summary_plot <- reactive( {
   biblioverlap::matching_summary_plot(calculate_results()$summary,
                                        text_size = input$summary_text_size,
                                        size = input$summary_value_size)
  })

  venn_plot <- reactive( {
    biblioverlap::venn_plot(calculate_results()$db_list,
                                    label = input$venn_label,
                                    label_color = input$venn_label_color,
                                    label_size = input$venn_label_size,
                                    label_alpha = input$venn_label_alpha,
                                    set_size = input$venn_set_size )
    } )

  # Upset plots from UpSetR can hide its empty intersections if a NULL value is passed to its `empty.intersections` parameter
  # However, the NULL value can not be passed directly to the `selectInput()` shiny function
  # So, the `selectInput` accepts an input from user and, if FALSE, will convert it to NULL in this reactive expression
  upset_empty_intersections <- reactive({
    if (input$empty.intersections) { return(TRUE) }
    else {return(NULL)}
  })

  upset_plot <- reactive ({
    req(input$order.by)
    biblioverlap::upset_plot(calculate_results()$db_list,
                             nsets = length(calculate_results()$db_list),
                             nintersects = input$nintersects,
                             order.by = input$order.by,
                             scale.intersections	= input$scale,
                             empty.intersections = upset_empty_intersections(),
                             scale.sets = input$scale,
                             text.scale = input$text_size,
                             show.numbers = input$show.numbers,
                             mb.ratio = c(input$mb.ratio, 1 - input$mb.ratio)
                             )
    }  )

  #Displaying plots in shinyApp (renderPlot functions)

  output$summary_plot <- renderPlot({
    summary_plot()
  }, height = reactive( { input$plot_height } ),
  width = reactive( { input$plot_width } )
  )

  output$venn <- renderPlot({
    venn_plot()
  }, height = reactive( { input$plot_height } ),
  width = reactive( { input$plot_width } )
  )

  output$upset <- renderPlot(   {
    upset_plot()
  }, height = reactive( { input$plot_height } ),
  width = reactive( { input$plot_width } )
  )

  #Generating download handlers for each plot (downloadHandler functions)

  #function to generate download handlers for the plots
  download_plot <- function(name, plot_obj) {
    downloadHandler(
      filename = function() {
        name
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = plot_obj(),
                        limitsize = FALSE,
                        bg = 'white',
                        height = input$plot_height,
                        width = input$plot_width,
                        units = 'px', dpi = 'screen'
        )
      }
    )
  }


  output$download_summary <- download_plot('summary_plot.png', summary_plot)

  output$download_venn <- download_plot('venn_plot.png', venn_plot)

  output$download_upset <- download_plot('upset_plot.png', upset_plot)


  #Code for the 'Merge Files' tabset
  merged_input_files <- reactive({

    input_files <- input$unmerged_files$datapath
    sep <- input$unmerged_sep
    quote <- input$unmerged_quote

    merged_files <- biblioverlap::merge_input_files(input_files,
                                    sep = sep,
                                    quote = quote)
    return(merged_files)
  })



  output$download_merged_file <- downloadHandler(
    filename = function() {
      'merged_files.csv'
    },
    content = function(file) {
      write.csv(merged_input_files(), file, row.names = FALSE)
    }
  )

  output$merged_files_table <- DT::renderDataTable({
      table <- merged_input_files()
      return( read_datatable(table) )
    }, server = TRUE) ##Server is necessary because the db_list can be huge

}
