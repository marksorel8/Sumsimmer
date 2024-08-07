HCR_feedback_UI <- function(id,title= "Harvest control rule"){

  fluidPage(

  # Application title
  titlePanel(title),

  withMathJax(),  # Enable MathJax

  # Show plot
  mainPanel(  "The current harvest control rule specifies fixed rates in some lower abundance tiers, and in higher abundance tiers the allowable catch is calculated as:
              $$(scalar * run size - offset) * share$$",


              p(em("tiers"), "= the run size below which the rates in the row are applied"),
              p(em("rate"), "= if a value is entered in the rate column, that is the harvest rate for the tier. If the value is greater than 1, it is assumed to be a total allowable catch rather than a rate."),
              p(em("scalar"), "= if the allowable catch is a function of the run size, as in the two highest tiers of the current rule, this number is multiplied by the run size."),
              p(em("offset"), "= if the allowable catch is a function of the run size, as in the two highest tiers of the current rule, this number is subtracted from the scaled run size."),
              p(em("share"), "= if the allowable catch is a function of the run size, as in the two highest tiers of the current rule, this number is multipled by the scales run size less the offset."),
              DTOutput(NS(id,"my_datatable")),
              "Hit this button to refresh the plot after changing the harvest control rule. The denominator in the rates shown is the river mouth run size, which is different from what is used to calculate allowable impacts in the the current Agreement. River mouth run size plus PFMC non-treaty AEQ mortalities is used as the denominator in the current Agreement. The plots assume that PFMC AEQ non-treary mortality is at average rates.",
              br(),

              actionButton(NS(id,"go"),label = "Update harvest control rule plot"),
              checkboxInput(NS(id,"total_NT"), "Include PFMC", value = FALSE),
              "check box to include the PFMC ocean mortality that is included in non-treaty share. This has no effect on simulations.",

              plotOutput(NS(id,"my_plot")),
              "Hit this button to start population simulation and plot escapement and harvest. This will take a few seconds.",
              br(),
              actionButton(NS(id,"dosim1"),label = "Update simulation"),
              br(),
              "check box to use the broken stick model of allowed vs. realized harvest.",
              checkboxInput(NS(id,"option2"), "Use broken stick", value = FALSE),



              p(em("Harvest plot."),"Grey bars represent historical data. Gray shaded area represent the 95% prediction interval (i.e., from the 2.5% to 97.5% quantiles across 500 simulated population trajectories. The thick black line is the median across the 500 simulations, and the thin black line is one of the 500 simulations, included to show the interannual variability in the predictions. "),


              plotOutput(NS(id,"sim1_harv")),
              br(),
              p(em("Escapement plot."),"The purple and greem bars represent historical spawning escapement and natural origin broodstock collections. The grey shaded area and black lines represent simulated future spawning escapement plus broodstock collection.") ,
              plotOutput(NS(id,"sim1_esc")),
  )
)
}


HCR_feedback_server <- function(id,
                                treaty_tiers=c(16000,36250,50000,Inf,rep(NA,3)),
                                treaty_rates=c(.05,.1,NA,NA,rep(NA,3)),
                                treaty_scalar=c(NA,NA,1,.75,rep(NA,3)),
                                treaty_offset=c(NA,NA,29000,16500,rep(NA,3)),
                                treaty_share = c(NA,NA,.5,.5,rep(NA,3)),
                                NT_tiers=c(5000,16000,29000,32000,36250,50001,Inf),
                                NT_rates=c(100,200,.05,.06,.07,NA,NA),
                                NT_scalar=c(rep(NA,5),1,.75),
                                NT_offset=c(rep(NA,5),29000,16500),
                                NT_share=c(rep(NA,5),.5,.5)){

  moduleServer(id, function(input, output, session) {

  #initialize a blank dataframe
  v <- reactiveValues(data = {
    data.frame(treaty_tiers = treaty_tiers,
               treaty_rates = treaty_rates,
               treaty_scalar = treaty_scalar,
               treaty_offset = treaty_offset,
               treaty_share = treaty_share,
               NT_tiers = NT_tiers,
               NT_rates = NT_rates,
               NT_scalar = NT_scalar,
               NT_offset = NT_offset,
               NT_share = NT_share)
  })

  #output the datatable based on the dataframe (and make it editable)
  output$my_datatable <- renderDT({
    DT::datatable(v$data, editable = TRUE, filter="none",
                  options = list(dom = 't',
                                 ordering=F,
                                 scrollX=FALSE,
                                 headerCallback = JS(
                                   "function(thead, data, start, end, display){",
                                   "  var $ths = $(thead).find('th');",
                                   "  $ths.each(function(){",
                                   "    var $th = $(this);",
                                   "    var text = $th.text();",
                                   "    $th.html(text.replace(/_/g, '<br>'));",  # Replace "_" with line break
                                   "  });",
                                   "}"
                                 ),
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().container()).css({'font-size': '10px'});",
                                   "}"
                                 )
                  ))
  })

  #when there is any edit to a cell, write that edit to the initial dataframe
  #check to make sure it's positive, if not convert
  observeEvent(input$my_datatable_cell_edit, {
    #get values
    info = input$my_datatable_cell_edit
    i = as.numeric(info$row)
    j = as.numeric(info$col)
    k <- info$value
    # Check if the cell is empty
    # Check if the cell is empty
    if (is.null(k) || k == "") {
      v$data[i, j] <- NA
    } else {
      # Convert to numeric and handle negative values
      k <- as.numeric(k)
      if (k < 0) {
        k <- k * -1
      }
      v$data[i, j] <- k
    }
  })



hcr_data_fun<-function(do_notifs=FALSE){
  hcr_data<-with(isolate(v$data),
                 seq_HCR(
                   treaty_tiers=treaty_tiers,
                   treaty_rates=treaty_rates,
                   treaty_scalar=treaty_scalar,
                   treaty_offset=treaty_offset,
                   treaty_share=treaty_share,
                   NT_tiers=NT_tiers,
                   NT_rates=NT_rates,
                   NT_scalar=NT_scalar,
                   NT_offset=NT_offset,
                   NT_share=NT_share
                 )
  )

  if(inherits(hcr_data, "error")){
    if(do_notifs)showNotification(hcr_data$message, type = "error")
    NULL
  }else{
    hcr_data
  }
}




  # Create a reactive value to store the simulation outputs
  hcr_out <- reactiveVal({
    hcr_data_fun()
  }
  )

  # Observe the button click event to call the function and store its output
  observeEvent(input$go, {
    req(input$go)
    hcr_out(hcr_data_fun(do_notifs=TRUE))

  })

  #render plot
  output$my_plot <- renderPlot({
    req(hcr_out())
    plot_HCR(hcr_out(),
             input$total_NT

    )

  })



  sim_data<-function(do_notifs=FALSE,harv_mod=1){
    newData <-  with(isolate(v$data),
                     pop_sim(
                       treaty_tiers=treaty_tiers,
                       treaty_rates=treaty_rates,
                       treaty_scalar=treaty_scalar,
                       treaty_offset=treaty_offset,
                       treaty_share=treaty_share,
                       NT_tiers=NT_tiers,
                       NT_rates=NT_rates,
                       NT_scalar=NT_scalar,
                       NT_offset=NT_offset,
                       NT_share=NT_share,
                       in_river_harvest_model_option = harv_mod
                     )
    )

    if(inherits(newData, "error")){
     if(do_notifs) showNotification(newData$message, type = "error")
      NULL
    }else{
      (newData)
    }

  }


  # Create a reactive value to store the simulation outputs
  sim1 <- reactiveVal(
    sim_data()
  )


  # Observe the button click event to call the function and store its output
  observeEvent(input$dosim1, {
    req(input$dosim1)

      sim1( sim_data(do_notifs=TRUE,harv_mod=ifelse(input$option2,2,1)))



  })


  output$sim1_esc <- renderPlot({
    req(sim1()) # Render the first plot using the stored data

    plot_esc_trajectory(sim1())
    # isolate(v$data) %>%  #don't react to any changes in the data
    #   ggplot(aes(x,y)) +
    #   geom_point() +
    #   geom_smooth(method = "lm")
  })

  output$sim1_harv <- renderPlot({
    req(sim1()) # Render the first plot using the stored data

    plot_harvest_trajectory(sim1())
    # isolate(v$data) %>%  #don't react to any changes in the data
    #   ggplot(aes(x,y)) +
    #   geom_point() +
    #   geom_smooth(method = "lm")
  })

  return(list(sim=sim1,
              hcr = hcr_out))

})
}
