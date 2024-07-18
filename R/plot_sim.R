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

quants_of_ave<-function(x,HCR_name="Current",fun=geo_mean,yrs=7:31){
  data.frame(cbind(
    apply(apply(x[,yrs,],c(1,3),fun),1,quantile),
    Total=quantile(apply(apply(x[,yrs,],2:3,sum),2,fun))))|>
    t() |> data.frame() |>
    `colnames<-`(c("min","LQI","med","UQI","max")) |>
    rownames_to_column(rnames)|>
    mutate(HCR=HCR_name)
}


harvest_quants_fun<-function(dat,HCR_name="Current",fun=geo_mean,qtiles=c(.025,.25,.5,.75,.975),yrs=7:31,...){
data.frame(rbind(
    apply(apply(apply(dat$terminal_NT[,yrs,],2:3,sum),2,quantile,qtiles,...),1,fun), # sum across populations, quantile across years, geometrtic mean across simulations
    apply(apply(apply(dat$terminal_NT[,yrs,],2:3,sum) + dat$PFMC[yrs,],2,quantile,qtiles,...),1,fun),
    apply(apply(apply(dat$terminal_treaty[,yrs,],2:3,sum),2,quantile,qtiles,...),1,fun)
  )) |>
    `colnames<-`(c("min","LQI","med","UQI","max")) |>
    mutate(Sector=c("NT_in_river","NT_plus_PFMC","Treaty"),
           HCR=HCR_name)
}


PNI_quants_fun<-function(dat,HCR_name="Current",rnames="River",qtiles=c(.025,.25,.5,.75,.975),yrs=7:31,...){

  ##pHOS
  sim_pHOS<-1-(dat$NOS[,yrs,]/dat$S[-1,yrs,])
  sim_pHOS_tot<-1-(apply(dat$NOS[,yrs,],2:3,sum)/apply(dat$S[-1,yrs,],2:3,sum))

  # pHOS_quants<-
  #   data.frame(cbind(
  #     apply(apply(sim_pHOS,c(1,3),quantile,qtiles),1:2,mean),
  #     Total=apply(apply(sim_pHOS_tot,2,quantile,qtiles),1,mean)))
  #
  # Mean_pHOS_quants<-
  #   data.frame(cbind(
  #     apply(apply(sim_pHOS,c(1,3),mean),1,quantile,qtiles),
  #     Total=quantile(apply(sim_pHOS_tot,2,mean),qtiles)))

  ##pNOB
  sim_pNOB<-dat$NOB[,yrs,]/(dat$HOB[-1,yrs,]+dat$NOB[,yrs,])
  sim_pNOB_tot<-apply(dat$NOB[,yrs,],2:3,sum)/apply((dat$HOB[-1,yrs,]+dat$NOB[,yrs,]),2:3,sum)

  # pNOB_quants<-   data.frame(cbind(
  #   apply(apply(sim_pNOB,c(1,3),quantile,qtiles),1:2,mean),
  #   Total=apply(apply(sim_pNOB_tot,2,quantile,qtiles),1,mean)))
  #
  # Mean_pNOB_quants<-
  #   data.frame(cbind(
  #     apply(apply(sim_pNOB,c(1,3),mean),1,quantile,qtiles),
  #     Total=quantile(apply(sim_pNOB_tot,2,mean),qtiles)))

  ##PNI
  sim_PNI<-sim_pNOB/(sim_pNOB+sim_pHOS)
  sim_PNI_tot<-sim_pNOB_tot/(sim_pNOB_tot+sim_pHOS_tot)

  PNI_quants<-
    data.frame(cbind(
      apply(apply(sim_PNI,c(1,3),quantile,qtiles),1:2,mean),
      Total=apply(apply(sim_PNI_tot,2,quantile,qtiles),1,mean)))

  # Mean_PNI_quants<-
  #   data.frame(cbind(
  #     apply(apply(sim_PNI,c(1,3),mean),1,quantile,qtiles),
  #     Total=quantile(apply(sim_PNI_tot,2,mean),qtiles)))

  # list(
  #   pHOS_quants=pHOS_quants,
  #      Mean_pHOS_quants=Mean_pHOS_quants,
  #      pNOB_quants=pNOB_quants,
  #      Mean_pNOB_quants=Mean_pNOB_quants,
  #      PNI_quants=PNI_quants,
  #      Mean_PNI_quants=Mean_PNI_quants
  #      ) |> lapply(function(x){x |> t()|>
  #         `colnames<-`(c("min","LQI","med","UQI","max")) |>
  #          data.frame() |>
  #          rownames_to_column(rnames) |>
  #          mutate( HCR=HCR_name)})
  PNI_quants |> t()|>
          `colnames<-`(c("min","LQI","med","UQI","max")) |>
           data.frame() |>
           rownames_to_column(rnames) |>
           mutate( HCR=HCR_name)

}



plot_harvest_quants<-function(harvest_quants){

  harvest_quants |>
    ggplot(aes(x = Sector, ymin = `min`, lower = `LQI`, middle = `med`, upper = `UQI`, ymax = `max`,fill=HCR))+geom_boxplot(stat="identity")+ylab("Annual harvest")

}

plot_NOS_quants<-function(NOS_quants){
  NOS_quants |>
  ggplot(aes(x = River, ymin = `min`, lower = `LQI`, middle = `med`, upper = `UQI`, ymax = `max`,fill=HCR))+geom_boxplot(stat="identity")+ylab("Annual natural-origin spawners")#+geom_hline(yintercept=50,lty=2)

}

plot_PNI_quants<-function(PNI_quants){
  PNI_quants|>
    ggplot(aes(x = River, ymin = `min`, lower = `LQI`, middle = `med`, upper = `UQI`, ymax = `max`,fill=HCR))+geom_boxplot(stat="identity")+ylab("Annual Proportionate Natural Influence")

}



plot_all_fun<-function(sim_list){

  # sim<-pop_sim()
  # sim2<-pop_sim(NO_broodstock_target =c(Methow = 50, Okanogan = 50, Wenatchee = 50))
  # sim_list<-list(Current=sim,weird=sim2)
  # library(tidyverse)
  harvest_quants<-do.call(rbind,lapply(names(sim_list),function(x){harvest_quants_fun(sim_list[[x]],HCR_name=x)}))

  NOS_quants<-do.call(rbind,lapply(names(sim_list),function(x) ave_quants(sim_list[[x]]$NOS,rnames="River",HCR_name=x))) |> mutate(River=fct_relevel(River,c("Wenatchee","Methow","Okanogan","Total")))

  PNI_quants<-do.call(rbind,lapply(names(sim_list),function(x) PNI_quants_fun(sim_list[[x]],rnames="River",HCR_name=x)))|> mutate(River=fct_relevel(River,c("Wenatchee","Methow","Okanogan","Total")))


  harv_plot<-plot_harvest_quants(harvest_quants)

  NOS_plot<-plot_NOS_quants(NOS_quants)

  PNI_plot<-plot_PNI_quants(PNI_quants)

  ggpubr::ggarrange(harv_plot,NOS_plot,PNI_plot,nrow=1,common.legend = TRUE, legend = "top")

}

# p1<-plot_all_fun(sim_list)
