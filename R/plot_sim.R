# sim<-pop_sim( SR_err=internal_data$SR_err[,,])

CV<-function(x){(sd(x)/mean(x))*100}


geo_mean<-function(x){exp(mean(log(x+1)))-1}

ave_quants<-function(x,HCR_name="Current",fun=mean,yrs=7:31,rnames,qtiles=c(0,.25,.5,.75,1),add_NO_tot=FALSE){

 df_out<- data.frame(
    apply(apply(x[,yrs,],c(1,3),quantile,qtiles),1:2,fun),
    Total=apply(apply(apply(x[,yrs,],2:3,sum),2,quantile,qtiles),1,fun))

  if(add_NO_tot){
    df_out<-data.frame(df_out,
                  "Total_natural"=
                    apply(apply(apply(x[-1,yrs,],2:3,sum),2,quantile,qtiles),1,fun))
  }

 df_out |>
     t() |> data.frame() |>
    `colnames<-`(c("min","LQI","med","UQI","max"))  |>
      rownames_to_column(rnames)|>
    mutate(HCR=HCR_name) |>
   drop_na()

}

quants_of_ave<-function(x,HCR_name="Current",fun=mean,yrs=7:31,rnames,qtiles=c(0,.25,.5,.75,1),add_NO_tot=FALSE){
  df_out<- data.frame(cbind(
    apply(apply(x[,yrs,],c(1,3),fun),1,quantile,qtiles),
    Total=quantile(apply(apply(x[,yrs,],2:3,sum),2,fun),qtiles)))

  if(add_NO_tot){
    df_out<-data.frame(df_out,
                       "Total_natural"=
                         apply(apply(x[-1,yrs,],c(1,3),fun),1,quantile,qtiles))
  }

  df_out|>
    t() |> data.frame() |>
    `colnames<-`(c("min","LQI","med","UQI","max")) |>
    rownames_to_column(rnames)|>
    mutate(HCR=HCR_name)
}

min_abund_func<-function(x,HCR_name="Current",fun=mean,yrs=7:31,rnames,MAT=c(1000,2000,1000)){
  data.frame(p_MA=c(
    apply(apply(x[-1,yrs,],c(1,3),fun)>=MAT,1,mean),
    NO_total= mean(apply(apply(x[-1,yrs,],c(1,3),fun),2,sum)>=sum(MAT))))|>
    # t() |> data.frame() |>
    rownames_to_column(rnames)|>
    mutate(HCR=HCR_name)
}



#QET
QET_fun<-function(x,rnames="Population",HCR_name="Current",QET=50){
x[,7:31,] |> apply(c(1,3),\(x)zoo::rollmean(x,k=4)) |> apply(2:3,\(x)min(x)<=QET) |> apply(1,mean) |> as.data.frame() |> `colnames<-`("pQET") |>
    rownames_to_column(rnames)|>
    mutate(HCR=HCR_name)
}




summarize_sim<-function(sim,yrs=7:31,HCR="X",MAT=c(1000,2000,1000)){
 esc_t<- esc_traj_data_fun(sim)
  harv_t<- harv_traj_dat_fun(sim)

  list(
#Escapement
Esc=ave_quants(sim$escapement,rnames="Population",HCR_name=HCR,add_NO_tot=TRUE),

Esc_mean=quants_of_ave(sim$escapement,rnames="Population",HCR_name=HCR),

MAT=min_abund_func(sim$escapement,rnames="Population",HCR_name=HCR,MAT=MAT),

QET=QET_fun(sim$escapement,rnames="Population",HCR_name=HCR,QET=50),


#NOS
NOS=ave_quants(sim$NOS,rnames="River",HCR_name=HCR),

#RMRS
RMRS=ave_quants(sim$returns,rnames="Population",HCR_name=HCR),

#S
S=ave_quants(sim$S,rnames="River",HCR_name=HCR),


#Harvest
Harv=harvest_quants_fun(sim,HCR_name=HCR),

Mean_harv=mean_harvest_quants_fun(sim,HCR_name=HCR),

Low_harv=low_harvest_quants_fun(sim,HCR_name=HCR),


#pHOS, pNOB, PNI
Hatch=hatchery_quants_fun(sim,HCR),

#hatchery surplus
H_surplus=hatchery_surplus_quants(sim,HCR_name=HCR),


esc_t=esc_t,
harv_t=harv_t,
HCR=sim$HCR
)
}



# #pQET
# sim$NOS[,7:31,] |> apply(c(1,3),zoo::rollmean,k=5) |> apply(2:3,function(x){min(x)<100}) |> apply(1,mean)



plot_all_fun<-function(sim_list,baseline_name,guide_rows=2,colors_vec){

  # sim<-pop_sim()
  # sim2<-pop_sim(,NT_scalar=c(rep(NA,5),1,1),treaty_scalar=c(NA,NA,1,1,rep(NA,3)))
  # sim3<-pop_sim(treaty_tiers = rep(NA,7),treaty_rates=rep(0,7),
                # NT_tiers = rep(NA,7),NT_rates=rep(0,7))
  # sim_list<-list(summarize_sim(sim,HCR="a") ,summarize_sim(sim2,HCR="b"))
  # library(tidyverse)

  # Combine lists by name
  combined_list <- lapply(unique(names(unlist(sim_list, recursive = FALSE))), function(name) {
    do.call(rbind, lapply(sim_list, function(lst) lst[[name]]))
  })

  # Assign names to the combined list
  names(combined_list) <- unique(names(unlist(sim_list, recursive = FALSE)))

  combined_list<-lapply(combined_list,function(x)dplyr::mutate(x,HCR=forcats::fct_inorder(HCR)))

  #
list(
  #Escapement
  NOE_plot=plot_NOE_quants(combined_list$Esc_mean,colors_vec = colors_vec),
  ## escapement in two rows for report
  NOE_plot_2row=plot_NOE_quants(combined_list$Esc_mean,2,colors_vec = colors_vec),


  # NOE_ratios_plot=plot_NOE_ratios(combined_list$Esc_mean,baseline_name),

  MAT_plot=plot_MAT(combined_list$MAT,colors_vec = colors_vec),

  QET_plot=plot_QET(combined_list$QET,colors_vec = colors_vec),

  #NOS
  NOS_plot=plot_spawner_quants(combined_list$NOS,ylab="Natural-origin spawners",colors_vec = colors_vec),



  #RMRS
  RMRS_plot=plot_pop_quants(combined_list$RMRS,"River mouth return",colors_vec = colors_vec),

  #S
  spawners_plot=plot_spawner_quants(combined_list$NOS,ylab="Total spawners (Hatchery + Wild)",colors_vec = colors_vec),


  #Harvest
  harv_plot=plot_harvest_quants(combined_list$Harv,guide_rows=guide_rows,colors_vec = colors_vec[-1]),
  Mean_harv_plot=plot_mean_harvest_quants(combined_list$Mean_harv,guide_rows=guide_rows,colors_vec = colors_vec[-1]),
  Low_harv_plot=plot_low_harvest_quants(combined_list$Low_harv,guide_rows=guide_rows,colors_vec = colors_vec[-1]),


  #pHOS, pNOB, PNI
   hatch_plot= plot_hatchery_quants(combined_list$Hatch,colors_vec = colors_vec),
  hatch_plot_2row= plot_hatchery_quants(combined_list$Hatch,2,colors_vec = colors_vec),


h_surplus = plot_hatchery_surplus(combined_list$H_surplus,colors_vec = colors_vec),
combined_list=combined_list
)
}




#
#
# plot_all_fun<-function(sim_list){
#
#   # sim<-pop_sim()
#   # sim2<-pop_sim(,NT_scalar=c(rep(NA,5),1,1),treaty_scalar=c(NA,NA,1,1,rep(NA,3)))
#   # sim_list<-list(Current=sim,weird=sim2)
#   # library(tidyverse)
#   harvest_quants<-do.call(rbind,lapply(names(sim_list),function(x){harvest_quants_fun(sim_list[[x]],HCR_name=x)})) |>
#         mutate(HCR=fct_relevel(HCR,names(sim_list)))
#
#   # NOS_quants<-do.call(rbind,lapply(names(sim_list),function(x) ave_quants(sim_list[[x]]$NOS,rnames="River",HCR_name=x))) |>
#   #   mutate(River=fct_relevel(River,c("Wenatchee","Methow","Okanogan","Total")),
#   #          HCR=fct_relevel(HCR,names(sim_list)))
#
#   NOE_quants<-do.call(rbind,lapply(names(sim_list),function(x) ave_quants(sim_list[[x]]$escapement,rnames="Population",HCR_name=x))) |>
#     mutate(Population=fct_relevel(Population,c("Hatchery","Wenatchee","Methow","Okanogan","Total")),
#            HCR=fct_relevel(HCR,names(sim_list)))
#
#
#
#   harv_plot<-plot_harvest_quants(harvest_quants)
#
#   # NOS_plot<-plot_NOS_quants(NOS_quants)
#
#   NOE_plot<-plot_NOE_quants(NOE_quants)
#
#
#   ggpubr::ggarrange(harv_plot,NOE_plot,nrow=1,common.legend = FALSE, legend = "top")
#
# }
#
