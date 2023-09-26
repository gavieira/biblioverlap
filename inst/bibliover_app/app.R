library(shiny)

# Define UI with fluid rows and columns
ui <- fluidPage(
  titlePanel('Biblioverlap'),
  tags$style(HTML(".compute_button { background-color: green; color: white; }")),
  sidebarLayout(
    sidebarPanel(

      HTML('Adding column names:'),

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

      numericInput('n_threads', 'Number of Threads',
                   min = 1,
                   max = parallel::detectCores(),
                   value = 1
                   ),


      tags$hr(),

      HTML('Adding bibliographical datasets:'),
      br(),
      br(),

      HTML('Set1:'),
      tabsetPanel(
        id = "set1",
        tabPanel("Name",
                 textInput("name1", "Dataset name:")
                 ),
        tabPanel("Files",
                 fileInput("set1", "Upload files:", multiple = TRUE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv") )
                 ),
        tabPanel("Sep",
                 selectInput('sep1', 'Separator',
                             choices = c(Comma = ",",
                                        Semicolon = ";",
                                        Tab = "\t"),
                             selected = "," )
                 ),
        tabPanel("Quote",
                 selectInput('sep1', 'Quote type',
                             choices = c(None = "",
                                         "Double Quote" = '"',
                                         "Single Quote" = "'"),
                             selected = "" )
                 )
        ),
      HTML('Set2:'),
      tabsetPanel(
        id = "set2",
        tabPanel("Name",
                 textInput("name2", "Dataset name:")
                 ),
        tabPanel("Files",
                 fileInput("set2", "Upload files:", multiple = TRUE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv") )
                 ),
        tabPanel("Sep",
                 selectInput('sep2', 'Separator',
                             choices = c(Comma = ",",
                                        Semicolon = ";",
                                        Tab = "\t"),
                             selected = "," )
                 ),
        tabPanel("Quote",
                 selectInput('sep2', 'Quote type',
                             choices = c(None = "",
                                         "Double Quote" = '"',
                                         "Single Quote" = "'"),
                             selected = "" )
                 )
        ),

      actionButton('compute', "Compute", width = '100%', class = 'compute_button')
    ),
    mainPanel()
  )
)

# Define server function that does nothing
server <- function(input, output, session) {
  observeEvent





}

# Create the app object
shinyApp(ui = ui, server = server)
