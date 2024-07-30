library(shiny)
library(DT)
library(tidyverse)

# Main UI
ui <- fluidPage(
  navbarPage("Summer Chinook simulator",
             tabPanel("HCR 1", HCR_feedback_UI("page1","Harvest control rule # 1")),
             tabPanel("HCR 2", HCR_feedback_UI("page2","Harvest control rule # 2")),
             tabPanel("HCR 3", HCR_feedback_UI("page3","Harvest control rule # 3")),
             tabPanel("Comparison",
                      "Hit this button to plot a comparison of harvest control rules.",
                      br(),
                      actionButton("renderComparisonPlot", "Render Comparison Plot"),
                      br(),
                      "The three panels show box and whisker plots of, from left to right, annual harvest, natural-origin spawner abundance, and Proportionate Natural Influence. The whiskers of the boxplot span 95% quantile intervals, the box spans the 50% quantile interval, and the midline is the median. Quantiles were calculated for values across years for individual simulations, and then averaged across simulations.  For example, the lower end of the bars in the harvest panel represent the average across simulations of the 2 years (out of 25) with the lowest harvest, averaged across simulations. I summarized the results in this way to show what harvest and conservaiton metrics would look line in small run size years, average years, and large run size years.",
                      br(),
                      plotOutput("compare_HCRS"))

  )
)


# Main server
server <- function(input, output, session) {
  sim1<-HCR_feedback_server("page1")
  sim2<-HCR_feedback_server("page2")
  sim3<-HCR_feedback_server("page3")
  #Combine data from all pages and render a combined plot
  observeEvent(input$renderComparisonPlot, {
          sim_list<-list(HCR_1=sim1(),
                     HCR_2=sim2(),
                     HCR_3=sim3()
      )

    sim_list <- Filter(Negate(is.null), sim_list)

    # Check if any of the simulations have been created
    if (length(sim_list)==0) {
      showNotification("No simulations have been created yet. Please run simulations on any of the pages first.", type = "error")
    } else {
    output$compare_HCRS<-renderPlot({

      p1<-plot_all_fun(sim_list)

      p1
    })
    }
  })
}


# Run the application
shinyApp(ui = ui, server = server)
