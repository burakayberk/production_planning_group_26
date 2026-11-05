test_module_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Test Module"),
    
    sliderInput(
      ns("x"),
      "Multiplier",
      min = 1,
      max = 10,
      value = 5
    ),
    
    plotOutput(ns("grafik"))
  )
}

test_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$grafik <- renderPlot({
      plot(
        1:10,
        input$x * (1:10),
        type = "l",
        xlab = "X",
        ylab = "Y",
        main = "Test Module Plot"
      )
    })
    
  })
}