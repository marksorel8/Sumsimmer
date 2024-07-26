library(shiny)
library(DT)
library(tidyverse)

# Main UI
ui <- fluidPage(
  navbarPage("Multi-Page App",
             tabPanel("Page 1", mod_page_ui("page1")),
             tabPanel("Page 2", mod_page_ui("page2")),
             tabPanel("Page 3", mod_page_ui("page3"))
  )
)


# Main server
server <- function(input, output, session) {
  mod_page_server("page1")
  mod_page_server("page2")
  mod_page_server("page3")
}


# Run the application
shinyApp(ui = ui, server = server)
