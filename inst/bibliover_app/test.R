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

library(shiny)

ui <- fluidPage(
  textOutput("value_output"),
  actionButton("update_btn", "Update Value"),
  conditionalPanel(
    condition = 'output.button_pressed',
    HTML('This will show only when the button is pressed')
  )
)

server <- function(input, output, session) {
  # Initialize a reactiveVal to track button press
  button_pressed <- reactiveVal(FALSE)

  # Observe changes to button_pressed and update the condition
  observe({
    value <- button_pressed()
    output$button_pressed <- renderText({
      paste("Current Value:", value)
    })
  })

  # Event to update button_pressed when the button is pressed
  observeEvent(input$update_btn, {
    button_pressed(TRUE)
  })
}
outputOptions(output, "button_pressed", suspendWhenHidden = FALSE)

shinyApp(ui, server)
