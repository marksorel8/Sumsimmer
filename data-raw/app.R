library(shiny)
library(DT)
library(tidyverse)
if (!requireNamespace("Sumsimmer", quietly = TRUE)) {
  # Install the package from GitHub if it is not installed
  remotes::install_github("marksorel8/Sumsimmer")
}

library(Sumsimmer)
# Main UI
ui <- fluidPage(
  navbarPage("Summer Chinook simulator",
             tabPanel("Welcome",
                      h1("Welcome!"),
                      h3("This app is for comparing simulated outcomes of harvest control rules for the summer Chinook managment stock of the",
                         tags$a(href="https://critfc.org/wp-content/uploads/2022/05/2018-2027-USvOR.pdf","US v Oregon Mangement Agreement"),
                         "."),
                      br(),
                      p("The first 5 tabs define different harvest control rules and the final tab called", tags$i(style="color: blue;","Comparison"), "is for comparing performance metrics of the harvest control rules."),
                      tags$ul(
                        tags$li(tags$i(style="color: blue;","No harvest"), " - A baseline with no terminral harvest"),
                        tags$li(tags$i(style="color: blue;","Current MA"), " - The harvest control rule defined on page 29 and in table A2 of the 2018-2029 Management Agreement."),

                        tags$li(tags$i(style="color: blue;","Simplified MA"), "- This is a simplified version of the harvest control rule in the 2018-2029 Managment Agreement. There is a fixed low rate below a river mouth run size of 29,000 and returns above 29,000 are fully allocated in a 50/50 split between treaty and non-treaty fisheries."),
                        tags$li(tags$i(style="color: blue;","PST"), " - The harvest control rule defined in Annex IV Chapter 3 of the"  ,tags$a(href="https://www.psc.org/wp-admin/admin-ajax.php?juwpfisadmin=false&action=wpfd&task=file.download&wpfd_category_id=45&wpfd_file_id=2337&token=&preview=1","Pacific Salmon Treaty"),". This rule is based on an escapement goal of 12,143 with fishing at 85% of the 2009-2015 average rate in years when the escapement goal is not achieved."),
                        tags$li(tags$i(style="color: blue;","Custom"), " - This is a blank canvas for trying whatever you like. Maybe try a tiered approach with a couple of different fixed exploitation rates at different river mouth run sizes? Or whatever you want...")
                      ),

                      "The harvest control rule in the `no harvest` tab is not editable, but all the other ones are, in case you want to tweak them. Double click on a cell to edit it's values. The description of how the harvest control rules are defined based on the values in the table is repeated on each tab.",
                      br(),
                      br(),
                      "To update the results after modifying a harvest control rule, hit the `Update harvest control rule plot` and `Update simulation` buttons. It will take a few seconds for the simulation to run. The `Comparison` tab will automatically update after a few seconds.",
                      br(),
                      br(),
                      "Each tab will take a second to load when first opened, and the `Comparison` tab will take several seconds."

                      ),
             tabPanel("No harvest", Sumsimmer:::HCR_feedback_UI("No harvest","No terminal harvest")),
             tabPanel("Current MA", Sumsimmer:::HCR_feedback_UI("Current MA","Harvest control rule from 2018-2027 Agreement")),
             tabPanel("Simplified MA", Sumsimmer:::HCR_feedback_UI("Simplified MA","Simplified version of the rule from 2018-2027 Agreement")),
             tabPanel("PST", Sumsimmer:::HCR_feedback_UI("PST","Harvest control rule from Annex IV Chapter 3 of Pacific Salmon Treaty. Also used in Pacific Fishery Mangment Council")),
             tabPanel("Custom", Sumsimmer:::HCR_feedback_UI("Custom","")),
              # tabPanel("Alt 1", HCR_feedback_UI("page2","Alternative harvest control rule # 1")),
             # tabPanel("Alt 2", HCR_feedback_UI("page3","Alternative harvest control rule # 2")),
             tabPanel("Comparison",
                      h1("This tab is for comparing harvest control rules"),
                      "It will take a few second for the plots to load upon starting the app.",
                      "The Harvest and Escapement plots are the main ones to look at. Other plots are included for a more nuanced comparison. Additional metrics could be added.",
                      "The plots will automatically update after a few seconds when a simulation is updated; however, you must press the buttons on the individual harvest-control-rule tabs after making changes for them to be updated here.",
                      # "Hit this button to plot a comparison of harvest control rules.",
                      # br(),
                      # actionButton("compareHCR", "Update harvest control rule plot"),
                      # br(),
                      # p(style="text-align:center",
                        # h2("Harvest control rules")),
                      br(),
                      br(),
                      plotOutput("compare_HCRs"),
                      p(tags$b("Harvest control rule comparison plot.")," Harvest rates for different sectors and all sectors combined under alternative harvest control rules. The denominator in the rates shown is the", em("river mouth run size"), "which is different from what is used to calculate allowable impacts in the the current Agreement. River mouth run size plus PFMC non-treaty AEQ mortalities is used as the denominator in the current Agreement. The plots assume that PFMC AEQ non-treary mortality is at average rates."),
             br(),
                      # p(style="text-align:center",
                      #   h2("Harvest and escapement")),
                      # actionButton("comparePerformance", "Render/update performance metrics comparison plot"),
                      # br(),

                      br(),
                      plotOutput("compare_perf_metrics"),
             p(tags$b("Harvest and escapement figure."), " The left panel shows box plots of annual harvest and the right 4 panels show boxplots of natural-origin escapement, which includes spawning, hatchery broodstock, and hatchery surplus. The whiskers of the boxplot span 95% quantile intervals, the box spans the 50% quantile interval, and the midline is the median. Quantiles were calculated across years for individual simulations, and then averaged across simulations.  Therefore, the lower end of the bars represent the average across simulations of the 2 years (out of 25) with the lowest harvest. I summarized the results in this way to show what harvest and escapement projections were in small run size, average, and large run size years."),

             br(),
             # p(style="text-align:center",
             #   h2("Integrated hatchery program performance")),
             # actionButton("comparePerformance", "Render/update performance metrics comparison plot"),
             br(),

             br(),
             plotOutput("hatchery_perf_metrics"),
             p(tags$b("Integrated hatchery program performance figure.")," Proportion of hatchery origin spawners (pHOS), proporiton of natural origin broodstock (pNOB) and proportionate natural influence (PNI= pNOB/(pNOB+pHOS)).Quantiles were calculated across years for individual simulations, and then averaged across simulations.The whiskers of the boxplot span 95% quantile intervals, the box spans the 50% quantile interval, and the midline is the median."),
             br(),
             p(style="text-align:center",
               h2("Extra plots")),
             # actionButton("comparePerformance", "Render/update performance metrics comparison plot"),
             br(),


             plotOutput("extra_perf_plot"),
             p(tags$b("Ratio of escapement to unfished escapement, and river mouth run size."), " Bars in the left panel show the ratio of geometric mean escapement between a given harvest control rule and a no-terminal-fishing control rule. Boxplots in the right panel show quantiles of River Mouth Run sizes. Note, the river mouth run size can increase with some harvest due to overcompensation (i.e., decreasing productivty at higher spawner abundances) in the Ricker model."),
             # p(style="text-align:center",
             #   h4("Spawners plots")),
             # actionButton("comparePerformance", "Render/update performance metrics comparison plot"),
             br(),

             plotOutput("compare_spawners_plot"),
             p(tags$b("Spawners figures.")," Quantile of natural-origin (left panel) and total (right panel) spawners across years of simulations.")
             )


             )

             )





# Main server
server <- function(input, output, session) {
  no_harv<-Sumsimmer:::HCR_feedback_server("No harvest",
                               editable=FALSE,
                               treaty_tiers=NA,
                                 treaty_rates=0,
                                 treaty_scalar=NA,
                                 treaty_offset=NA,
                                 treaty_share=NA,
                               NT_tiers=NA,
                                 NT_rates=0,
                                 NT_scalar=NA,
                                 NT_offset=NA,
                                 NT_share=NA)

  current<-Sumsimmer:::HCR_feedback_server("Current MA",editable=TRUE)

  PST<-do.call(Sumsimmer:::HCR_feedback_server,(c(id="PST",Sumsimmer:::internal_data$`PST`$perf$HCR,editable=TRUE)))

  simple_ma<-do.call(Sumsimmer:::HCR_feedback_server,(c(id="Simplified MA",Sumsimmer:::internal_data$`Simplified MA`$perf$HCR,editable=TRUE)))




  # sim2<-HCR_feedback_server("page2",
  #                           treaty_tiers=c(36250,rep(NA,2)),
  #                           treaty_rates=c(.1,rep(NA,2)),
  #                           treaty_scalar=c(NA,1,rep(NA,1)),
  #                           treaty_offset=c(NA,29000,rep(NA,1)),
  #                           treaty_share = c(NA,.5,rep(NA,1)),
  #                           NT_tiers=c(32222,rep(NA,2)),
  #                           NT_rates=c(.05,rep(NA,2)),
  #                           NT_scalar=c(NA,1,rep(NA,1)),
  #                           NT_offset=c(NA,29000,rep(NA,1)),
  #                           NT_share = c(NA,.5,rep(NA,1)))
  sim3<-Sumsimmer:::HCR_feedback_server("Custom",
                            treaty_tiers=rep(NA,7),
                            treaty_rates=rep(NA,7),
                            treaty_scalar=rep(NA,7),
                            treaty_offset=rep(NA,7),
                            treaty_share = rep(NA,7),
                            NT_tiers=rep(NA,7),
                            NT_rates=rep(NA,7),
                            NT_scalar=rep(NA,7),
                            NT_offset=rep(NA,7),
                            NT_share = rep(NA,7),
                            editable=TRUE)
  #Combine data from all pages and render a combined plot

render_HCR_compare<-function(){
  hcr_list<-list("No harvest"=no_harv$hcr(),
                  "Current MA"=current$hcr(),
                 "Simplified MA"=simple_ma$hcr(),
                 "PST" = PST$hcr(),
                 "Custom" = sim3$hcr()

                  )#,
                 # "Alt 1"=sim2$hcr(),
                 # "Alt 2"=sim3$hcr()
  # )

  hcr_list <- Filter(Negate(is.null), hcr_list)



  perf_list<-list(no_harv$sim()[1:7],
                  current$sim()[1:7],
                  simple_ma$sim()[1:7],
                  PST$sim()[1:7],
                  sim3$sim()[1:7]
  )#,
  # "Alt 1"=sim2$sim1(),
  # "Alt 2"=sim3$sim1()
  # )

  perf_list <- Filter(Negate(is.null), perf_list)

    # Check if any of the HCRs have been created
  if(length(hcr_list)==0){
    showNotification("please render the harvest control rule plots on all of the individual pages before comparing them here.", type = "error")
  }else{

    output$compare_HCRs<-renderPlot({

      p2<-Sumsimmer:::plot_HCR_compare(hcr_list)

      p2
    })

output$compare_perf_metrics<-renderPlot({

  ggpubr::ggarrange(plots$harv_plot,plots$NOE_plot,nrow=1,common.legend = FALSE, legend = "top",widths=c(1.2,2))

})
#
plots<-Sumsimmer:::plot_all_fun(perf_list,"No harvest")
output$compare_perf_metrics<-renderPlot({

  ggpubr::ggarrange(plots$harv_plot,plots$NOE_plot,nrow=1,common.legend = FALSE, legend = "top",widths=c(1,2))

})

output$compare_spawners_plot <-renderPlot({

  ggpubr::ggarrange(plots$NOS_plot,plots$spawners_plot,nrow=1,common.legend = TRUE, legend = "top",widths=c(1,1))

})


output$extra_perf_plot <-renderPlot({

  ggpubr::ggarrange(plots$NOE_ratios_plot,plots$RMRS_plot,nrow=1,common.legend = TRUE, legend = "top",widths=c(1.3,2))

})

output$hatchery_perf_metrics<-renderPlot({
  plots[["hatch_plot"]]

})





  }
}

observe({
  render_HCR_compare()
})

#   observeEvent(input$compareHCR, {
#
#     render_HCR_compare()
# })


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
