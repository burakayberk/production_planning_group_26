
library(shiny)

ui <- navbarPage("Production Planning",
  tabPanel("Learning Curve",
           learning_curve_ui("learning_curve")),

  tabPanel("Moving Average",
           moving_average_ui("moving_average")),

  tabPanel("Test Module",
           test_module_ui("test_module")),

  tabPanel("Weighted Moving Average",
           weighted_moving_average_ui("weighted_moving_average"))
)

server <- function(input, output, session) {
  learning_curve_server("learning_curve")
  moving_average_server("moving_average")
  test_module_server("test_module")
  weighted_moving_average_server("weighted_moving_average")
}

shinyApp(ui, server)
