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
                      h3("This app is for comparing simulated outcomes of harvest control rules for upper Columbia River summer Chinook"),
                      h3(tags$i(style="color: red;","This app does not represent agency policy positions nor managment decisions, has not been endorsed by any agency, and should not be interpreted as such.")),
                      br(),
                      p("The first 5 tabs define different harvest control rules and the final tab called", tags$i(style="color: blue;","Comparison"), "is for comparing performance metrics of the harvest control rules."),
                      tags$ul(
                        tags$li(tags$i(style="color: blue;","No harvest"), " - A baseline with no terminral harvest"),
                        tags$li(tags$i(style="color: blue;","Current MA"), " - The harvest control rule defined on page 29 and in table A2 of the 2018-2029 Management Agreement."),

                        tags$li(tags$i(style="color: blue;","PST"), " - The harvest control rule defined in Annex IV Chapter 3 of the"  ,tags$a(href="https://www.psc.org/wp-admin/admin-ajax.php?juwpfisadmin=false&action=wpfd&task=file.download&wpfd_category_id=45&wpfd_file_id=2337&token=&preview=1","Pacific Salmon Treaty"),". This rule is based on an escapement goal of 12,143 with fishing at 85% of the 2009-2015 average rate in years when the escapement goal is not achieved."),
                        tags$li(tags$i(style="color: blue;","Custom 1 & 2"), " - These are blank canvases for trying whatever you like. Maybe try a tiered approach with a couple of different fixed exploitation rates at different river mouth run sizes? Or whatever you want...")
                      ),

                      "The harvest control rule in the `no harvest` tab is not editable, but all the other ones are, in case you want to tweak them. Double click on a cell to edit it's values. ",
                      br(),
                      br(),
                      "To update the results after modifying a harvest control rule, hit the `Update harvest control rule plot` and `Update simulation` buttons. It will take a few seconds (about 10 on my laptop) for the simulation to run. The `Comparison` tab will automatically update.",
                      br(),
                      br(),
                      "Each tab will take a second to load when first opened, and the `Comparison` tab will take several seconds."

                      ),
             tabPanel("No harvest", Sumsimmer:::HCR_feedback_UI("No_harvest","No terminal harvest")),
             tabPanel("Current MA", Sumsimmer:::HCR_feedback_UI("Current_MA","Harvest control rule from 2018-2027 Agreement")),
             # tabPanel("Simplified MA", Sumsimmer:::HCR_feedback_UI("Simplified MA","Simplified version of the rule from 2018-2027 Agreement")),
             tabPanel("PST", Sumsimmer:::HCR_feedback_UI("PST","Harvest control rule from Annex IV Chapter 3 of Pacific Salmon Treaty. Also used in Pacific Fishery Mangment Council")),
             tabPanel("Custom 1", Sumsimmer:::HCR_feedback_UI("Custom_1","")),
             tabPanel("Custom 2", Sumsimmer:::HCR_feedback_UI("Custom_2","")),
              # tabPanel("Alt 1", HCR_feedback_UI("page2","Alternative harvest control rule # 1")),
             # tabPanel("Alt 2", HCR_feedback_UI("page3","Alternative harvest control rule # 2")),
             tabPanel("Comparison",
                      h1("This tab is for comparing harvest control rules"),
                      "It will take a few second for the plots to load upon starting the app.",
                      "The Harvest and Escapement plots are the main ones to look at. Other plots are included for a more nuanced comparison. Additional metrics could be added.",
                      "The plots will automatically update after a few seconds when a simulation is updated; however, you must press the buttons on the individual harvest-control-rule tabs after making changes for them to be updated here.",
                      br(),
       "You can download a report of the results by hitting this button.",

                      downloadButton("Report", "Generate report"),
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
             p(tags$b("Spawners figures.")," Quantile of natural-origin (left panel) and total (right panel) spawners across years of simulations."),
             br(),
             br(),
             plotOutput("hatchery_surplus"),
             p(tags$b("Hatchery surplus figures.")," Quantiles of number of hatchery origin returns that were not harvested nor needed for broodstock.")
             )


             )

             )





# Main server
server <- function(input, output, session) {
  # no_harv<-Sumsimmer:::HCR_feedback_server(id="No harvest",
  #                              editable=FALSE,
  #                              treaty_tiers=NA,
  #                                treaty_rates=0,
  #                                treaty_scalar=NA,
  #                                treaty_offset=NA,
  #                                treaty_share=NA,
  #                              NT_tiers=NA,
  #                                NT_rates=0,
  #                                NT_scalar=NA,
  #                                NT_offset=NA,
  #                                NT_share=NA,
  #                              pfmc_cutoff=100000)
  no_harv<-do.call(Sumsimmer:::HCR_feedback_server,(c(id="No_harvest",Sumsimmer:::internal_data$`No harvest`$perf$HCR,editable=TRUE)))

  current<-do.call(Sumsimmer:::HCR_feedback_server,(c(id="Current_MA",Sumsimmer:::internal_data$`Current MA`$perf$HCR,editable=TRUE)))
    # Sumsimmer:::HCR_feedback_server(id="Current MA",editable=TRUE, pfmc_cutoff=29000)

  PST<-do.call(Sumsimmer:::HCR_feedback_server,(c(id="PST",Sumsimmer:::internal_data$`PST`$perf$HCR,editable=TRUE)))

  # simple_ma<-do.call(Sumsimmer:::HCR_feedback_server,(c(id="Simplified MA",Sumsimmer:::internal_data$`Simplified MA`$perf$HCR,editable=TRUE)))


  sim3<-Sumsimmer:::HCR_feedback_server(id="Custom_1",
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
                            pfmc_cutoff=0,
                            editable=TRUE,
                            custom=TRUE)

  sim4<-Sumsimmer:::HCR_feedback_server("Custom_2",
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
                                        pfmc_cutoff=0,
                                        editable=TRUE,
                                        custom=TRUE)

  #Combine data from all pages and render a combined plot

render_HCR_compare<-function(){
  hcr_list<-list("No harvest"=no_harv$hcr(),
                  "Current MA"=current$hcr(),
                 # "Simplified MA"=simple_ma$hcr(),
                 "PST" = PST$hcr(),
                 "Custom 1" = sim3$hcr(),
                 "Custom 2" = sim4$hcr()

                  )

  hcr_list <- Filter(Negate(is.null), hcr_list)



  perf_list<-list(no_harv$sim()[1:8],
                  current$sim()[1:8],
                  # simple_ma$sim()[1:8],
                  PST$sim()[1:8],
                  sim3$sim()[1:8],
                  sim4$sim()[1:8]
  )

  perf_list <- Filter(Negate(is.null), perf_list)

    # Check if any of the HCRs have been created
  if(length(hcr_list)==0){
    showNotification("please render the harvest control rule plots on all of the individual pages before comparing them here.", type = "error")
  }else{
  p2<-Sumsimmer:::plot_HCR_compare(hcr_list)
    output$compare_HCRs<-renderPlot({
      p2
    })

# output$compare_perf_metrics<-renderPlot({
#
#   ggpubr::ggarrange(plots$harv_plot,plots$NOE_plot,nrow=1,common.legend = FALSE, legend = "top",widths=c(1.2,2))
#
# })
#
plots<-Sumsimmer:::plot_all_fun(perf_list,"No harvest")

compare_perf_metrics<-ggpubr::ggarrange(plots$harv_plot,plots$NOE_plot,nrow=1,common.legend = FALSE, legend = "top",widths=c(1,2))

output$compare_perf_metrics<-renderPlot({

  compare_perf_metrics
})

compare_spawners_plot<-ggpubr::ggarrange(plots$NOS_plot,plots$spawners_plot,nrow=1,common.legend = TRUE, legend = "top",widths=c(1,1))

output$compare_spawners_plot <-renderPlot({

  compare_spawners_plot

})

extra_perf_plot<-ggpubr::ggarrange(plots$NOE_ratios_plot,plots$RMRS_plot,nrow=1,common.legend = TRUE, legend = "top",widths=c(1.3,2))

output$extra_perf_plot <-renderPlot({

  extra_perf_plot

})


output$hatchery_perf_metrics<-renderPlot({
  plots[["hatch_plot"]]

})

output$hatchery_surplus<-renderPlot({
  plots[["h_surplus"]]

})




  }

return(list(
  # compare_perf_metrics = compare_perf_metrics,
  # compare_spawners_plot = compare_spawners_plot,
  # extra_perf_plot = extra_perf_plot,
  # hatchery_perf_metrics=plots[["hatch_plot"]],
  # hatchery_surplus = plots[["h_surplus"]],
  hcr_plot= p2,
  plots=plots,
  hcr_list=hcr_list
))
}

stored_value <- reactiveVal(NULL)

observe({
out <-  render_HCR_compare()
stored_value(out)
})


output$Report <- downloadHandler(
  # For PDF output, change this to "report.pdf"
  filename = "Report.docx",
  content = function(file) {
    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy("report.Rmd", tempReport, overwrite = TRUE)

    # Set up parameters to pass to Rmd document
    params <- list(hcr_plot = stored_value()$hcr_plot,
                   harv_plot = stored_value()$plots$harv_plot,
                   NOE_plot = stored_value()$plots$NOE_plot_2row,
                   NOS_plot = stored_value()$plots$NOS_plot,
                   spawners_plot = stored_value()$plots$spawners_plot,
                   NOE_ratios_plot = stored_value()$plots$NOE_ratios_plot,
                   RMRS_plot = stored_value()$plots$RMRS_plot,
                   hatch_plot = stored_value()$plots$hatch_plot_2row,
                   h_surplus = stored_value()$plots$h_surplus)

    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)

}


# Run the application
shinyApp(ui = ui, server = server)
