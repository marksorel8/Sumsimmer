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
    ggplot(aes(x = River, ymin = `min`, lower = `LQI`, middle = `med`, upper = `UQI`, ymax = `max`,fill=HCR))+geom_boxplot(stat="identity")+ylab("Annual Proportionate Natural Influence")+ylim(0,1)

}



plot_all_fun<-function(sim_list){

  # sim<-pop_sim()
  # sim2<-pop_sim(,NT_scalar=c(rep(NA,5),1,1),treaty_scalar=c(NA,NA,1,1,rep(NA,3)))
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


plot_HCR<-function(treaty_tiers=c(16000,36250,50000,Inf),
                   treaty_rates=c(.05,.1,NA,NA),
                   treaty_scalar=c(NA,NA,1,.75),
                   treaty_offset=c(NA,NA,29000,16500),
                   treaty_share = c(NA,NA,.5,.5),
                   NT_tiers=c(5000,16000,29000,32000,36250,50001,Inf),
                   NT_rates=c(100,200,.05,.06,.07,NA,NA),
                   NT_scalar=c(rep(NA,5),1,.75),
                   NT_offset=c(rep(NA,5),29000,16500),
                   NT_share=c(rep(NA,5),.5,.5)
                   ){

  RMRS<-seq(5000,75000,length.out=100)
  PFMC<-exp(internal_data$PFMC_ave_prop*log(RMRS))
  Treaty<-numeric(100)
  for(i in 1:100){Treaty[i]<-allowed_Treaty(RMRS[i],PFMC[i],
                                            treaty_tiers,
                                            treaty_rates,
                                            treaty_scalar,
                                            treaty_offset,
                                            treaty_share)}
  NT_in_river<-numeric(100)
  for(i in 1:100){NT_in_river[i]<-allowed_NT(RMRS[i],PFMC[i],
                                             NT_tiers,
                                             NT_rates,
                                             NT_scalar,
                                             NT_offset,
                                             NT_share)}


  plot(RMRS,Treaty,type="l",lwd=2,ylab="Harvest rate (in river)",ylim=c(0,max(c(Treaty,NT_in_river)*1.15)))
  points(RMRS,NT_in_river,type="l",col="firebrick4",lwd=2)
legend("topleft",c("Treaty","Non-treaty"),lty=1,col=c("black","firebrick4"))
}



#plot_HCR()


plot_esc_trajectory<-function(sim,yrs=7:31){
  escapement_sim<-sim$NOS+sim$NOB

  escapement_sim_quants<-escapement_sim[,yrs,] |> apply(1:2,quantile,c(.025,.25,.5,.75,.975)) |> array2DF() |> `colnames<-`(c("quant","population_name","year","NO_Return")) |> dplyr::mutate(name="Escapement",year=as.numeric(year))|> tidyr::pivot_wider(names_from = quant,values_from = NO_Return) |> dplyr::rename(NO_Return=`50%`)


  #plot
  internal_data$esc_dat %>% dplyr::filter(!is.na(`SG No. Age 4`),year<=2022) |> dplyr::select(year,population_name ,Spawn=NOS,Broodstock=NOBroodStockRemoved) %>%
    tidyr::drop_na() |> tidyr::pivot_longer(c(Spawn,Broodstock),values_to = "NO_Return") %>% ggplot(aes(x=year,y=NO_Return,fill=name))+geom_bar(stat="identity")+facet_wrap(~population_name,scales="free_x")+theme(legend.position = "top")+ylab("Nat. Origin Escapement")+geom_ribbon(data=escapement_sim_quants,aes(x=year,ymin=`2.5%`,ymax=`97.5%`),fill="grey",)+geom_line(data=escapement_sim_quants,aes(x=year,y=NO_Return),lwd=2)+geom_line(data=(escapement_sim[,yrs,15] |> array2DF()|> `colnames<-`(c("population_name","year","NO_Return"))|> dplyr::mutate(name="Escapement",year=as.numeric(year))),aes(x=year,y=NO_Return) )

}


plot_harvest_trajectory<-function(sim,yrs=7:31){
  terminal_harv<-apply(sim$terminal_NT[,yrs,],2:3,sum) |>  apply(1,quantile,c(.025,.25,.5,.75,.975)) |> t() |> tibble::as_tibble() |> dplyr::mutate(Sector="Non-treaty",year=2023:2047) |>
    dplyr::bind_rows(
      apply(sim$terminal_treaty[,yrs,],2:3,sum) |>  apply(1,quantile,c(.025,.25,.5,.75,.975)) |> t() |> tibble::as_tibble() |> dplyr::mutate(Sector="Treaty",year=2023:2047)
    ) |> dplyr::rename(In_river_harvest=`50%`)

  internal_data$pfmc_morts_2 |> dplyr::mutate(Sector=ifelse(Sector=="NT","Non-treaty",Sector)) |> dplyr::filter(Year<=2022) |> ggplot(aes(x=Year,y=In_river_harvest))+geom_col()+facet_wrap(~Sector)+ylab("In-river harvest")+
    geom_ribbon(data=terminal_harv,aes(x=year,ymin=`2.5%`,ymax=`97.5%`),fill="grey")+
    geom_line(data=terminal_harv,aes(x=year,y=In_river_harvest),lwd=2)+
    geom_line(data=tibble::tibble(In_river_harvest=c(apply(sim$terminal_NT[,yrs,],2:3,sum)[,15],
                                             apply(sim$terminal_treaty[,yrs,],2:3,sum)[,15]),
                          year=rep(2023:2047,time=2),
                          Sector=rep(c("Non-treaty","Treaty"),each=25)),aes(x=year,y=In_river_harvest))
  }


# plot_esc_trajectory(sim)
# plot_harvest_trajectory(sim)
