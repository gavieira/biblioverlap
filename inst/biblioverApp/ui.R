source('ui_components.R', local = TRUE)

ui <- fluidPage(
  custom_styles,
  favicon,
  title = 'Biblioverlap',
  navbarPage('Biblioverlap',
            id = 'tabs',
    tabPanel('Input Data',
      sidebarLayout(
        sidebarPanel( width = 3,
          input_data_panel_title,
          column_names_ui,
          tags$hr(),
          score_matching_options_ui,
          tags$hr(),
          n_threads_ui,
          tags$hr(),
          n_sets_ui,
          actionButton('submit', "Submit", width = '100%', class = 'custom_button')
        ),
        mainPanel( width = 9,
          conditionalPanel(
            condition = "output.calculation_done",
          tabsetPanel(
            id = "results",
            results_data,
            results_summary_table )
          )
          )
        )
      ),
    tabPanel('Plots',
             sidebarLayout(
               sidebarPanel( width = 3,
                             plot_panel_title,
                             plot_settings
                             ),
               mainPanel( width = 9,
                          conditionalPanel(
                            condition = "output.calculation_done",
                            tabsetPanel(
                              id = 'plots',
                              results_summary_plot,
                              results_venn,
                              results_upset )
                            )
                          )
               )
             ),

    tabPanel('Merge Files',
             merge_files_ui
             )
    )
)
