library(shiny)
library(DT)
library(tidyverse)

# Main UI
ui <- fluidPage(
  navbarPage("Summer Chinook simulator",
             tabPanel("Current MA", HCR_feedback_UI("page1","Harvest control rule from current MA"))#,
             # tabPanel("Alt 1", HCR_feedback_UI("page2","Alternative harvest control rule # 1")),
             # tabPanel("Alt 2", HCR_feedback_UI("page3","Alternative harvest control rule # 2")),
             # tabPanel("Comparison",
             #          # "Hit this button to plot a comparison of harvest control rules.",
             #          # br(),
             #          # actionButton("compareHCR", "Update harvest control rule plot"),
             #          # br(),
             #          p(style="text-align:center",
             #            h2("Harvest control rules")),
             #          "Harvest rates for different sectors and all sectors compused under alternative harvest control rules. The denominator in the rates shown is the river mouth run size, which is different from what is used to calculate allowable impacts in the the current Agreement. River mouth run size plus PFMC non-treaty AEQ mortalities is used as the denominator in the current Agreement. The plots assume that PFMC AEQ non-treary mortality is at average rates.",
             #          br(),
             #          plotOutput("compare_HCRs"),
             #          br(),
             #          p(style="text-align:center",
             #            h2("Harvest and escapement")),
             #          # actionButton("comparePerformance", "Render/update performance metrics comparison plot"),
             #          # br(),
             #          "The left panel shows box plots of annual harvest and the right 4 panels show boxplots of natural-origin escapement, which includes spawning, hatchery broodstock, and hatchery surplus. The whiskers of the boxplot span 95% quantile intervals, the box spans the 50% quantile interval, and the midline is the median. Quantiles were calculated for values across years for individual simulations, and then averaged across simulations.  For example, the lower end of the bars in the harvest panel represent the average across simulations of the 2 years (out of 25) with the lowest harvest, averaged across simulations. I summarized the results in this way to show what harvest and conservaiton metrics would look line in small run size years, average years, and large run size years.",
             #          br(),
             #          plotOutput("compare_perf_metrics"),
             #
             # br(),
             # p(style="text-align:center",
             #   h2("Integrated hatchery program performance")),
             # # actionButton("comparePerformance", "Render/update performance metrics comparison plot"),
             # # br(),
             # "Proportion of hatchery origin spawners (pHOS), prooriton of natural origin broodstock (pNOB) and proportionate natural influence (PNI= pNOB/(pNOB+pHOS)).The whiskers of the boxplot span 95% quantile intervals, the box spans the 50% quantile interval, and the midline is the median. Quantiles were calculated for values across years for individual simulations, and then averaged across simulations.",
             # br(),
             # plotOutput("hatchery_perf_metrics"))

             )

  )



# Main server
server <- function(input, output, session) {
  sim1<-HCR_feedback_server("page1")
  # sim2<-HCR_feedback_server("page2",
  #                           treaty_tiers=c(36250,rep(NA,6)),
  #                           treaty_rates=c(.1,rep(NA,6)),
  #                           treaty_scalar=c(NA,1,rep(NA,5)),
  #                           treaty_offset=c(NA,29000,rep(NA,5)),
  #                           treaty_share = c(NA,.5,rep(NA,5)),
  #                           NT_tiers=c(32222,rep(NA,6)),
  #                           NT_rates=c(.05,rep(NA,6)),
  #                           NT_scalar=c(NA,1,rep(NA,5)),
  #                           NT_offset=c(NA,29000,rep(NA,5)),
  #                           NT_share = c(NA,.5,rep(NA,5)))
  # sim3<-HCR_feedback_server("page3",
  #                           treaty_tiers=rep(NA,7),
  #                           treaty_rates=rep(NA,7),
  #                           treaty_scalar=rep(NA,7),
  #                           treaty_offset=rep(NA,7),
  #                           treaty_share = rep(NA,7),
  #                           NT_tiers=rep(NA,7),
  #                           NT_rates=rep(NA,7),
  #                           NT_scalar=rep(NA,7),
  #                           NT_offset=rep(NA,7),
  #                           NT_share = rep(NA,7))
  #Combine data from all pages and render a combined plot

# render_HCR_compare<-function(){
#   hcr_list<-list("Current MA"=sim1$hcr(),
#                  "Alt 1"=sim2$hcr(),
#                  "Alt 2"=sim3$hcr()
#   )
#
#   hcr_list <- Filter(Negate(is.null), hcr_list)
#
#   # Check if any of the HCRs have been created
#   if(length(hcr_list)==0){
#     showNotification("please render the harvest control rule plots on all of the individual pages before comparing them here.", type = "error")
#   }else{
#
#     output$compare_HCRs<-renderPlot({
#
#       p2<-plot_HCR_compare(hcr_list)
#
#       p2
#     })
#   }
# }
#
# observe({
#   render_HCR_compare()
# })
#
# #   observeEvent(input$compareHCR, {
# #
# #     render_HCR_compare()
# # })
#
#
# render_performance_plot<-function(){
#
#   sim_list<-list("Current MA"=sim1$sim(),
#                  "Alt 1"=sim2$sim(),
#                  "Alt 2"=sim3$sim()
#   )
#
#   sim_list <- Filter(Negate(is.null), sim_list)
#
#
#   # Check if any of the simulations have been created
#   if (length(sim_list)==0) {
#     showNotification("No simulations have been created yet.", type = "error")
#   }else{
#     output$compare_perf_metrics<-renderPlot({
#
#       p1<-plot_all_fun(sim_list)
#
#       p1
#     })
#
#     output$hatchery_perf_metrics<-renderPlot({
#
#       p2<-plot_hatchery_quants(sim_list)
#
#       p2
#     })
#
#   }
# }
#
#
# observe({
#   render_performance_plot()
# })
}


#   observeEvent(input$comparePerformance, {
#
#     render_performance_plot()
# })




# Run the application
shinyApp(ui = ui, server = server)
