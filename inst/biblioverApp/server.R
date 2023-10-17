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


  read_input_file <- function(input_file, sep) {
      df <- read.csv(input_file,
                     sep = sep,
                     strip.white = TRUE,
                     check.names = FALSE)
    return( df )
  }


  # Function to retrieve information from all sets
  get_sets_list <- function() {
    all_sets <- list()
    for (i in 1:input$n_sets) {
      set_name <- input[[paste0("name", i)]]
      filepath <- input[[paste0("file", i)]]$datapath
      sep <- input[[paste0("sep", i)]]

      # Read the CSV file and store it in a data frame
      df <- read_input_file(filepath, sep)

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
          tabPanel("File",
                   fileInput(paste0("file", id), "Upload file:",
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
      'summary.jpg'
    },
    content = function(file) {
      #ggplot2::ggsave(file, plot = calculate_results()$summary$plot, device = "png")
      ggplot2::ggsave(file, plot = update_summary_plot(), device = "png")
      #png(file)
      #print(calculate_results()$summary$plot)
      #dev.off()
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


  update_summary_plot <- reactive({
    summary_plot <- calculate_results()$summary$plot
    summary_plot <- summary_plot + ggplot2::update_geom_defaults("text", list(size = 5)) + ggplot2::theme(text=ggplot2::element_text(size=15))
    return( summary_plot )
  })


  output$summary_plot <- renderPlot({
    update_summary_plot()
  })


  #output$summary_plot <- renderPlot({
  #  summary_plot <- calculate_results()$summary$plot
  #  summary_plot <- summary_plot + ggplot2::update_geom_defaults("text", list(size = 5)) + ggplot2::theme(text=ggplot2::element_text(size=15))
  #  return( summary_plot )
  #})

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


  #observeEvent(input$merge_button)
  #observe({
  #  View(input$unmerged_files)
  #  print(input$unmerged_files$datapath)
  #  print(typeof(input$unmerged_files$datapath))
  #  })

  # Function to merge multiple input_files
  merge_input_files <- reactive({
    input_files <- input$unmerged_files$datapath
    sep <- input$unmerged_sep

    df_list <- lapply(input_files, function(input_file) {
      read.csv(input_file,
               sep = sep,
               strip.white = TRUE,
               check.names = FALSE) })
    df <- do.call(rbind, df_list)
    df <- df[!duplicated(df), ]   # Removing duplicate records

    return( df )
  })

  output$download_merged_file <- downloadHandler(
    filename = function() {
      'merged_data.csv'
    },
    content = function(file) {
      write.csv(merge_input_files(), file, row.names = FALSE)
    }
  )

  output$merged_files_table <- DT::renderDataTable({
      table <- merge_input_files()
      return( read_datatable(table) )
    }, server = TRUE) ##Server is necessary because the db_list can be huge




}