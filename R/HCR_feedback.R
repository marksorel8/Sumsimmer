HCR_feedback_UI <- function(id,title= "Harvest control rule",wen_alph=reactive(3)){

  fluidPage(

    # Application title
    titlePanel(title),

    withMathJax(),  # Enable MathJax

    # Show plot
    mainPanel(  h3("Harvest control rule definition"),
                "The current harvest control rule specifies fixed rates numbers or rates in lower abundance tiers, and in higher abundance tiers the allowable catch is calculated as:
              $$(scalar * run size - offset) * share$$",


                p(em("tiers"), "= the run size below which the rates in the row are applied"),
                p(em("rate"), "= if a value is entered in the rate column, that is the harvest rate for the tier. If the value is greater than 1, it is assumed to be a total allowable catch rather than a rate."),
                p(em("scalar"), "= if the allowable catch is a function of the run size, as in the two highest tiers of the current rule, this number is multiplied by the run size."),
                p(em("offset"), "= if the allowable catch is a function of the run size, as in the two highest tiers of the current rule, this number is subtracted from the scaled run size."),
                p(em("share"), "= if the allowable catch is a function of the run size, as in the two highest tiers of the current rule, this number is multipled by the scales run size less the offset."),
                br(),
                p("The", em("run size"), "abundance index for the current harvest control rule is the total river mouth run size plus calendar year PFMC AEQ mortalities. Check this box to change the abundance index to the ",em("wild river mouth run size"), "which is wild escapement devided by one minus the wild terminal harvest rate. Click the ",em(" wild-only abundance"), "box to use the natural-origin escapement to the river mouth as the abundance index (does not include ocean morts)"),
                fluidRow(
                  column(4, numericInput(
                    NS(id,"pfmc_cutoff_id"),
                    "Run size above which PFMC AEQ is included in the Non-treaty allocation",
                    value=10
                  )),
                  column(4,checkboxInput(NS(id,"wild_AI"), "Use wild-only abundance index", value = FALSE))),

                br(),

                DTOutput(NS(id,"my_datatable")),
                "Hit this button to refresh the plot after changing the harvest control rule. The denominator in the rates shown is the river mouth run size, which is different from what is used to calculate allowable impacts in the the current Agreement. River mouth run size plus PFMC non-treaty AEQ mortalities is used as the denominator in the current Agreement. The plots assume that PFMC AEQ non-treary mortality is 8.2% of the river mouth run size.",
                br(),
                h3("Harvest control rule plot"),
                actionButton(NS(id,"go"),label = "Update harvest control rule plot"),
                checkboxInput(NS(id,"total_NT"), "Include PFMC", value = FALSE),
                "check box to include the PFMC ocean mortality that is included in non-treaty share. This has no effect on simulations.",

                plotOutput(NS(id,"my_plot")),
                p(tags$b("Harvest control rule figure."), "Allowed in-river harvest rate across different river mouth runs izes (RMRS). The average PFMC AEQ mortality is used to calculate the allowed non-treaty rate when the RMRS > 29k. The `Include PFMC` button can be used to include the PFMC mortalities in the harvest rate, although this doesn't make a lot of sense because the denominator in the harvest rates shown here is the River Mouth Run size. Note that the allowed harvest for the non-treaty sector can be nevative when the PFMC AEQ mortalities is greater than the allowed."),
                h3("Simulations plots"),
                "Uncheck box to assume no hatchery-origin spawners",
                checkboxInput(NS(id,"HOS_option"), "Hatchery-origin spawners", value = TRUE),
                br(),
                "Choices for model of allowed vs. realized harvest. The log-log model assumes that the average harvest will be less than the allowed, the broken-stick model assumes that in-river harvest is not responsive (due to managment error) at lower allowed harvest rates, and the average model assumes that harvest will be equal to allowed on average",
                selectInput(NS(id,"option2"),"Harvest implemention error model",
                            c("Log-log"=1,"Broken stick"=2,"Average"=3)),
                br(),
                "Scalar for implementation error in harvest (i.e., due to forecast and managment error). The defaults are .20 for treaty and 0.15 for non-treaty with the default model, and .21 for treaty and .18 for non-treaty with the broken hockey stick model. The below scalar is multiplied by those default values. ",
                numericInput(
                  NS(id,"IE"),
                  "Implementation error scalar",
                  value=1,
                  min = .05,
                  max = 5,
                  step = .1
                ),
                br(),
                # checkboxInput(NS(id,"option2"), "Use broken stick", value = FALSE),

                "This is the proportion of unmarked fish captured in the non-treaty fisheries that are released.",
                numericInput(
                  NS(id,"URR"),
                  "Non-treaty unmarked release rate",
                  value=0.88,
                  min = .05,
                  max = .95,
                  step = .01
                ),
                br(),
                # h3(p("Ricker ",em("\u03B1"), "parameter values")),
                # fluidRow(
                #   column(3,numericInput(
                #     NS(id,"Wen_alpha"),
                #     "Wenatchee",
                #     value=round(internal_data$alphas[4],3),
                #     min = .01,
                #     max = 25,
                #     step = .01
                #   )),
                #   column(3,numericInput(
                #     NS(id,"Met_alpha"),
                #     "Methow",
                #     value=round(internal_data$alphas[2],3),
                #     min = .01,
                #     max = 25,
                #     step = .01
                #   )),
                #   column(3,numericInput(
                #     NS(id,"Ok_alpha"),
                #     "Okanogan",
                #     value=round(internal_data$alphas[3],3),
                #     min = .01,
                #     max = 25,
                #     step = .01
                #   )),
                #   column(3,numericInput(
                #     NS(id,"Hatch_alpha"),
                #     "Hatchery",
                #     value=round(internal_data$alphas[1],3),
                #     min = .01,
                #     max = 100,
                #     step = .01
                #   ))
                # ),
                # h3(p("Ricker ",em("Rmax"), "parameter values")),
                # fluidRow(
                #   column(3,numericInput(
                #     NS(id,"Wen_Rmax"),
                #     "Wenatchee",
                #     value=round(internal_data$Rmax[3],3)
                #   )),
                #   column(3,numericInput(
                #     NS(id,"Met_Rmax"),
                #     "Methow",
                #     value=round(internal_data$Rmax[1],3)
                #   )),
                #   column(3,numericInput(
                #     NS(id,"Ok_Rmax"),
                #     "Okanogan",
                #     value=round(internal_data$Rmax[2],3)
                #   )),
                #   column(3,numericInput(
                #     NS(id,"Hatch_Rmax"),
                #     "Hatchery",
                #     value=1000000
                #   ))
                # ),


                "Hit this button to run the population simulation and plot escapement and harvest after changing the harvest control rule. This will take several seconds.",
                br(),
                actionButton(NS(id,"dosim1"),label = "Update simulation"),
                plotOutput(NS(id,"sim1_harv")),
                p(tags$b("Harvest figure."), " Grey bars represent historical data. Gray shaded area represent the 95% prediction interval (i.e., from the 2.5% to 97.5% quantiles across 500 simulated population trajectories. The thick black line is the median across the 500 simulations, and the thin black line is one of the 500 simulations, included to show the interannual variability of individual simulations."),


                br(),

                plotOutput(NS(id,"sim1_esc")),
                p(tags$b("Escapement figure.")," Bars represent historical escapement (Spawners, hatchery broodstock, and hatchery surplus). The grey shaded area and black lines represent simulated future escapements.")
    )
  )
}


HCR_feedback_server <- function(id,
                                index,
                                treaty_tiers=c(16000,36250,50000,Inf,rep(NA,3)),
                                treaty_rates=c(.05,.1,NA,NA,rep(NA,3)),
                                treaty_scalar=c(NA,NA,1,.75,rep(NA,3)),
                                treaty_offset=c(NA,NA,29000,16500,rep(NA,3)),
                                treaty_share = c(NA,NA,.5,.5,rep(NA,3)),
                                NT_tiers=c(5000,16000,29000,32000,36250,50001,Inf),
                                NT_rates=c(100,200,.05,.06,.07,NA,NA),
                                NT_scalar=c(rep(NA,5),1,.75),
                                NT_offset=c(rep(NA,5),29000,16500),
                                NT_share=c(rep(NA,5),.5,.5),
                                pfmc_cutoff,
                                # Wen_alpha=reactive(),
                                # Met_alpha=reactive(),
                                # Ok_alpha=reactive(),
                                # Hatch_alpha=reactive(),
                                # Wen_Rmax=reactive(),
                                # Met_Rmax=reactive(),
                                # Okan_Rmax=reactive(),
                                # Hatch_Rmax=reactive(),
                                update=reactive(),
                                editable=TRUE,
                                custom=FALSE){



  moduleServer(id, function(input, output, session) {


    # Update the default value for PFMC cutoff
    updateNumericInput(session, "pfmc_cutoff_id", value = pfmc_cutoff)

    updateCheckboxInput(session, "wild_AI", value = ifelse(index=="total",FALSE,TRUE))

#
#     observeEvent(update(),
#                  {
#       updateNumericInput(session, "Wen_alpha", value = Wen_alpha())
#       updateNumericInput(session, "Met_alpha", value = Met_alpha())
#       updateNumericInput(session, "Ok_alpha", value = Ok_alpha())
#       updateNumericInput(session, "Hatch_alpha", value = Hatch_alpha())
#       updateNumericInput(session, "Wen_Rmax", value = Wen_Rmax())
#       updateNumericInput(session, "Met_Rmax", value = Met_Rmax())
#       updateNumericInput(session, "Ok_Rmax", value = Okan_Rmax())
#       updateNumericInput(session, "Hatch_Rmax", value = Hatch_Rmax())
#
#
#       sim1( sim_data(do_notifs=TRUE,
#                      harv_mod=input$option2,
#                      HOS_model=ifelse(input$HOS_option,"HOE","zero"),
#                      URR=input$URR,
#                      IE=input$IE,
#                      index=ifelse(input$wild_AI,"wild","total"),
#                      PFMC_include_point=input$pfmc_cutoff_id,
#                      alpha=c(Hatch_alpha(),
#                              Met_alpha(),
#                              Ok_alpha(),
#                              Wen_alpha()
#                      ),
#                      Rmax= c(Hatch_Rmax(),
#                              Met_Rmax(),
#                              Okan_Rmax(),
#                              Wen_Rmax()
#                      )
#       ))
#
#
#
#     })



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
      DT::datatable(v$data, editable = editable, filter="none",
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



    hcr_data_fun<-function(do_notifs=FALSE,index_type,PFMC_include_point){
      hcr_data<-with(isolate(v$data),
                     Sumsimmer:::seq_HCR(
                       index=index_type,
                       pfmc_cutoff=PFMC_include_point,
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

      hcr<-with(isolate(v$data),
                list(
                  index=index_type,
                  pfmc_cutoff=PFMC_include_point,
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
        list(
        hcr_data=hcr_data,
        hcr=hcr
        )
      }
    }




    # Create a reactive value to store the simulation outputs
    hcr_out <- reactiveVal(NULL)

    # Update hcr_out based on input changes
    if(!custom){
      observe({
        hcr_out(
          hcr_data_fun(
            do_notifs = TRUE,
            index_type = index,
            PFMC_include_point = pfmc_cutoff
          )
        )
      })
    }

    #
    #   hcr_out <- reactiveVal({
    #     hcr_data_fun(do_notifs=TRUE,
    #                  index=ifelse(input$wild_AI,"wild","total"),
    #                  PFMC_include_point=input$pfmc_cutoff_id)
    #     # Sumsimmer:::internal_data[[id]]$hcr
    #   }
    #   )


    # Observe the button click event to call the function and store its output
    observeEvent(input$go, {
      req(input$go)
      hcr_out(hcr_data_fun(do_notifs=TRUE,
                           index_type=index,
                           PFMC_include_point=input$pfmc_cutoff_id))

    })

    #render plot
    output$my_plot <- renderPlot({
      req(hcr_out())
      Sumsimmer:::plot_HCR(hcr_out()$hcr_data,
                           input$total_NT
      )

    })



    sim_data<-function(do_notifs=FALSE,harv_mod,HOS_model,URR,IE,index,PFMC_include_point#,alpha,Rmax
    ){
      newData <-  with(isolate(v$data),
                       pop_sim(
                         index=index,
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
                         in_river_harvest_model_option = harv_mod,
                         HOS_model=HOS_model,
                         NT_Unmarked_release_rate=URR,
                         implementation_error_scalar=IE,
                         PFMC_include_above=PFMC_include_point#,
                         # alpha =alpha,
                         # Rmax =Rmax
                       )
      )

      if(inherits(newData, "error")){
        if(do_notifs) showNotification(newData$message, type = "error")
        NULL
      }else{




        Sumsimmer:::summarize_sim(newData,HCR=id)
      }

    }


    # Create a reactive value to store the simulation outputs
    sim1 <- reactiveVal(
        Sumsimmer:::internal_data[[id]]$perf
    )


    # Observe the button click event to call the function and store its output
    observeEvent(input$dosim1, {
      req(input$dosim1)

      sim1( sim_data(do_notifs=TRUE,
                     harv_mod=input$option2,
                     HOS_model=ifelse(input$HOS_option,"HOE","zero"),
                     URR=input$URR,
                     IE=input$IE,
                     index=ifelse(input$wild_AI,"wild","total"),
                     PFMC_include_point=input$pfmc_cutoff_id#,

      ))



    })


    output$sim1_esc <- renderPlot({
      req(sim1()) # Render the first plot using the stored data
      Sumsimmer:::plot_esc_trajectory( sim1()$esc_t)


    })

    output$sim1_harv <- renderPlot({
      req(sim1()) # Render the first plot using the stored data

      Sumsimmer:::plot_harvest_trajectory(sim1()$harv_t)


    })

    params_list<-reactiveVal(NULL)

    observe(
      params_list(list(
        harv_mod=input$option2,
        HOS_model=ifelse(input$HOS_option,"HOE","zero"),
        URR=input$URR,
        IE=input$IE
      ))
    )



    return(list(sim=sim1,
                hcr = hcr_out,
                params=params_list))

  })
}
