# UI code
ui <- fileInput("file_upload", "Upload a file")

# Server code
server <- function(input, output, session) {
  observeEvent(input$file_upload, {
    # Access the uploaded file information
    View(input$file_upload)
    uploaded_file <- input$file_upload

    # Check if a file was uploaded
    if (!is.null(uploaded_file)) {
      # Extract the file path
      file_path <- uploaded_file$datapath

      # You can now use file_path as the path to the uploaded file
      # For example, you can read the file using read.csv()
      data <- read.csv(file_path)

      # Do something with the data
      # For demonstration purposes, we are just printing the data
      print(data)
    }
  })
}

shinyApp(ui, server)
