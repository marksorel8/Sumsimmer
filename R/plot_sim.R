# sim<-pop_sim( SR_err=internal_data$SR_err[,,])

CV<-function(x){(sd(x)/mean(x))*100}


geo_mean<-function(x){exp(mean(log(x)))}

ave_quants<-function(x,HCR_name="Current",fun=geo_mean,yrs=7:31,rnames){
  data.frame(cbind(
    apply(apply(x[,yrs,],c(1,3),quantile),1:2,fun),
    Total=apply(apply(apply(x[,yrs,],2:3,sum),2,quantile),1,fun))) |>
     t() |> data.frame() |>
    `colnames<-`(c("min","LQI","med","UQI","max"))  |>
      rownames_to_column(rnames)|>
    mutate(HCR=HCR_name)

}

quants_of_ave<-function(x,HCR_name="Current",fun=geo_mean,yrs=7:31,rnames){
  data.frame(cbind(
    apply(apply(x[,yrs,],c(1,3),fun),1,quantile),
    Total=quantile(apply(apply(x[,yrs,],2:3,sum),2,fun))))|>
    t() |> data.frame() |>
    `colnames<-`(c("min","LQI","med","UQI","max")) |>
    rownames_to_column(rnames)|>
    mutate(HCR=HCR_name)
}


summarize_sim<-function(sim,yrs=7:31,HCR="X"){
 esc_t<- plot_esc_trajectory(sim)
  harv_t<- plot_harvest_trajectory(sim)

  list(
#Escapement
Esc=ave_quants(sim$escapement,rnames="Population",HCR_name=HCR),

#NOS
NOS=ave_quants(sim$NOS,rnames="River",HCR_name=HCR),

#RMRS
RMRS=ave_quants(sim$returns,rnames="Population",HCR_name=HCR),

#S
S=ave_quants(sim$S,rnames="River",HCR_name=HCR),


#Harvest
Harv=harvest_quants_fun(sim,HCR_name=HCR),

#pHOS, pNOB, PNI
Hatch=hatchery_quants_fun(sim,HCR)#,

# esc_t=esc_t,
# harv_t=harv_t
)
}



# #pQET
# sim$NOS[,7:31,] |> apply(c(1,3),zoo::rollmean,k=5) |> apply(2:3,function(x){min(x)<100}) |> apply(1,mean)


plot_all_fun<-function(sim_list){

  # sim<-pop_sim()
  # sim2<-pop_sim(,NT_scalar=c(rep(NA,5),1,1),treaty_scalar=c(NA,NA,1,1,rep(NA,3)))
  # sim3<-pop_sim(treaty_tiers = rep(NA,7),treaty_rates=rep(0,7),
                # NT_tiers = rep(NA,7),NT_rates=rep(0,7))
  # sim_list<-list(summarize_sim(sim,HCR="a") ,summarize_sim(sim2,HCR="b"),summarize_sim(sim3,HCR="H0"))
  # library(tidyverse)

  # Combine lists by name
  combined_list <- lapply(unique(names(unlist(sim_list, recursive = FALSE))), function(name) {
    do.call(rbind, lapply(sim_list, function(lst) lst[[name]]))
  })

  # Assign names to the combined list
  names(combined_list) <- unique(names(unlist(sim_list, recursive = FALSE)))

  # View the result

  #Escapement
  NOE_plot<-plot_NOE_quants(combined_list$Esc)

  NOE_ratios_plot<-plot_NOE_ratios(combined_list$Esc)



  #NOS
  NOS_plot<-plot_NOS_quants(combined_list$NOS)



  #RMRS
  # RMRS=ave_quants(sim$returns,rnames="Population",HCR_name=HCR)

  #S
  # S=ave_quants(sim$S,rnames="River",HCR_name=HCR),


  #Harvest
  harv_plot<-plot_harvest_quants(combined_list$Harv)


  #pHOS, pNOB, PNI
   hatch_plot<- plot_hatchery_quants(combined_list$Hatch)





}






plot_all_fun<-function(sim_list){

  # sim<-pop_sim()
  # sim2<-pop_sim(,NT_scalar=c(rep(NA,5),1,1),treaty_scalar=c(NA,NA,1,1,rep(NA,3)))
  # sim_list<-list(Current=sim,weird=sim2)
  # library(tidyverse)
  harvest_quants<-do.call(rbind,lapply(names(sim_list),function(x){harvest_quants_fun(sim_list[[x]],HCR_name=x)})) |>
        mutate(HCR=fct_relevel(HCR,names(sim_list)))

  # NOS_quants<-do.call(rbind,lapply(names(sim_list),function(x) ave_quants(sim_list[[x]]$NOS,rnames="River",HCR_name=x))) |>
  #   mutate(River=fct_relevel(River,c("Wenatchee","Methow","Okanogan","Total")),
  #          HCR=fct_relevel(HCR,names(sim_list)))

  NOE_quants<-do.call(rbind,lapply(names(sim_list),function(x) ave_quants(sim_list[[x]]$escapement,rnames="Population",HCR_name=x))) |>
    mutate(Population=fct_relevel(Population,c("Hatchery","Wenatchee","Methow","Okanogan","Total")),
           HCR=fct_relevel(HCR,names(sim_list)))



  harv_plot<-plot_harvest_quants(harvest_quants)

  # NOS_plot<-plot_NOS_quants(NOS_quants)

  NOE_plot<-plot_NOE_quants(NOE_quants)


  ggpubr::ggarrange(harv_plot,NOE_plot,nrow=1,common.legend = FALSE, legend = "top")

}

