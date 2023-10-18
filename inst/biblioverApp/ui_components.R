column_names_ui <-
  tagList(
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
    )
  )


score_matching_params_ui <-
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

score_matching_options_ui <-
  tagList(
    tags$strong('Score matching'),
    tags$br(),
    actionButton("change_score_params", "Change parameters", class = 'custom_button'),
    tags$br(),
    conditionalPanel(
      condition = "input.change_score_params % 2 == 1",
      tags$br(),
      score_matching_params_ui
      )
    )



n_threads_ui <- numericInput('n_threads', 'Number of threads',
                             min = 1,
                             max = parallel::detectCores(),
                             value = 1
)

n_sets_ui <-
  tagList(
    numericInput('n_sets', "Number of bibliographical datasets",
                          min = 2, max = 7,
                          value = 2),
    uiOutput("dynamicUI")
    )


results_data <-  tabPanel("Data",
                          tags$br(),
                          downloadButton("download_data", "Download Data", class = 'custom_button'),
                          tags$br(),
                          DT::dataTableOutput('full_table')
                          )


results_summary_table <- tabPanel("Summary",
                            tags$br(),
                            downloadButton("download_summary_table", "Download Summary Table", class = 'custom_button'),
                            tableOutput('summary_table')
                            )


results_summary_plot <- tabPanel("Summary",
                            tags$br(),
                            actionButton("modify_summary", "Modify plot", class = 'custom_button'),
                            conditionalPanel(
                              condition = "input.modify_summary % 2 == 1",
                              fluidRow(
                                column(2, numericInput('summary_value_size', 'Value size',
                                                       min = 0, max = 50,
                                                       value = 4.5, step = 0.5)
                                ),
                                column(2, numericInput('summary_text_size', 'text_size',
                                                       min = 0, max = 50,
                                                       value = 4.5, step = 0.05)
                                )
                              )
                            ),
                            plotOutput('summary_plot', width = '100%')
                            )


results_venn <- tabPanel("Venn Diagram",
                         tags$br(),
                         actionButton("modify_venn", "Modify plot", class = 'custom_button'),
                         conditionalPanel(
                           condition = "input.modify_venn % 2 == 1",
                           fluidRow(
                             column(2, selectInput('venn_label', 'Label type',
                                                   choices = c(Count = "count",
                                                               Percent = "percent",
                                                               Both = "both",
                                                               None = "none"),
                                                   selectize = FALSE,
                                                   selected = "both" )
                             ),
                             column(2, selectInput('venn_label_color', 'Label color',
                                                   choices = c(Black = "black",
                                                               White = "white"),
                                                   selectize = FALSE,
                                                   selected = "black" )
                             ),
                               column(2, numericInput('venn_label_size', 'Label size',
                                                    min = 1, max = 10,
                                                    value = 4.5, step = 0.5)
                             ),
                               column(2, numericInput('venn_label_alpha', 'Label alpha',
                                                    min = 0, max = 1,
                                                    value = 0.5, step = 0.05)
                             ),
                               column(2, numericInput('venn_set_size', 'Set size',
                                                    min = 1, max = 10,
                                                    value = 4.5, step = 0.5)
                             )
                           )
                         ),
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


merge_files_ui <- sidebarLayout(
                    sidebarPanel( width = 3,
                                  HTML('<br>Biblioverlap accepts a single csv file for each dataset.
                                  However, there are cases when a query has to be split between multiple files. <br> <br>
                                  In this page, the user can upload multiple csv files (from the same bibliographical database) and merge all records
                                  into a single file. <br> <br>'),
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
                                  tags$br(),
                                  downloadButton('download_merged_file', "Download Merged File", width = '100%', class = 'custom_button')
                    ),
                    mainPanel( width = 9,
                               DT::dataTableOutput('merged_files_table') )
                  )
