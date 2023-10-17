column_names_ui <-
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
  )


score_matching_options_ui <-
  tagList(
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
  )

n_threads_ui <- numericInput('n_threads', 'number of threads',
                             min = 1,
                             max = parallel::detectCores(),
                             value = 1
)

n_sets_ui <- numericInput('n_sets', "Number of bibliographical datasets",
                          min = 2, max = 7,
                          value = 2)


results_data <-  tabPanel("Data",
                          tags$br(),
                          downloadButton("download_data", "Download Data", class = 'custom_button'),
                          tags$br(),
                          DT::dataTableOutput('full_table')
                          )


results_summary <- tabPanel("Summary",
                      #Could have both a table and a plot, and donwload boxes for both of them.
                      tags$br(),
                      downloadButton("download_summary_table", "Download Summary Table", class = 'custom_button'),
                      #downloadButton("download_summary_plot", "Download Summary Plot", class = 'custom_button'),
                      tableOutput('summary_table'),
                      plotOutput('summary_plot', width = '100%')
                      #plotOutput('summary_plot', width = "1920px", height = "1080px")
                      )


results_venn <- tabPanel("Venn Diagram",
                         plotOutput('venn', width = '100%')
                         )


#Matybe split the results_upset into a part that contains the modify options and one for the plot itself
results_upset <-  tabPanel("UpSet Plot",
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
