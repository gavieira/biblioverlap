#genes <- paste("gene",1:1000,sep="")
#
#x <- list(Asdjfsadkfj = sample(genes,300),
#          Bsadjflsdajflj = sample(genes,525),
#          Cksdfjlksadjf =sample(genes,440),
#          Dsdfjsaldkfjklsa =sample(genes,350),
#          Eklasdjfklasdknvlk=sample(genes,200),
#          Fksadfjaklsdfkj=sample(genes,150),
#          Gklsadjfklsdajfkads=sample(genes,100))
#
#test <- ggVennDiagram::ggVennDiagram(x) +
#  ggplot2::scale_fill_gradient(low = "#A7C7E7", high = "#08306B") +
#  ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = .2))
#
#
#test
#
#
#test + ggplot2::guides(fill= ggplot2::guide_legend(title = 'New Fill Legend'))
#
##nintersects and text.scale (last value)
#UpSetR::upset(UpSetR::fromList(x),
#              nsets = 7,
#              nintersects = 100,
#              mainbar.y.label = "Document Intersection Size",
#              sets.x.label = "Bibliographic Set Size",
#              #keep.order = TRUE,
#              show.numbers = 'no',
#              set_size.show	= TRUE,
#              #set_size.angles	 = 90,
#              order.by = 'freq',
#              empty.intersections = 'on',
#              query.legend	= 'top',
#              #point.size = 4,
#              #line.size = 2,
#              text.scale = c(2, 2, 1.7, 1.5, 1.5, 2))
#

#library(shiny)
#
#ui <- fluidPage(
#  textOutput("value_output"),
#  actionButton("update_btn", "Update Value"),
#  conditionalPanel(
#    condition = 'output.button_pressed',
#    HTML('This will show only when the button is pressed')
#  )
#)
#
#server <- function(input, output, session) {
#  # Initialize a reactiveVal to track button press
#  button_pressed <- reactiveVal(FALSE)
#
#  # Observe changes to button_pressed and update the condition
#  observe({
#    value <- button_pressed()
#    output$button_pressed <- renderText({
#      paste("Current Value:", value)
#    })
#  })
#
#  # Event to update button_pressed when the button is pressed
#  observeEvent(input$update_btn, {
#    button_pressed(TRUE)
#  })
#}
#outputOptions(output, "button_pressed", suspendWhenHidden = FALSE)
#
#shinyApp(ui, server)
#


library(shiny)

ui <- fluidPage(
  titlePanel('File Merging and Download'),

  tabsetPanel(
    id = 'merging_user_input',
    tabPanel('Files',
             fileInput('unmerged_files', 'Upload files', multiple = TRUE,
                       accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
             actionButton('merge_button', "Merge Files", width = '100%', class = 'custom_button')
    ),
    tabPanel('Sep',
             selectInput('unmerged_sep', 'Separator',
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selectize = FALSE,
                         selected = ",")
    )
  ),
  downloadLink("download_merged_file", "Download Merged File")
)

server <- function(input, output, session) {
  # Define a reactive expression to handle file merging
  merged_data <- reactive({
    req(input$merge_button)  # Wait for the button click

    # Merge the files based on the selected separator
    merged <- NULL
    for (file in input$unmerged_files) {
      data <- read.csv(file$datapath, sep = input$unmerged_sep)
      merged <- rbind(merged, data)
    }
    return(merged)
  })

  # Generate the merged file for download
  output$download_merged_file <- downloadHandler(
    filename = function() {
      "merged_data.csv"  # Specify the filename for download
    },
    content = function(file) {
      write.csv(merged_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
