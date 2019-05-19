# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  observeEvent(input$do, {
    showModal(modalDialog(
      title = "All inputs",
      "Input summary will appear here!"
    ))
  })
}
