hatchery_quants_fun<-function(dat,HCR_name="Current",rnames="River",qtiles=c(.025,.25,.5,.75,.975),yrs=7:31,...){

  ##pHOS
  sim_pHOS<-dat$pHOS[,yrs,]#(dat$HOS[,yrs,]/(dat$HOS[-1,yrs,]+dat$NOS[-1,yrs,]))
  # sim_pHOS_tot<-1-(apply(dat$NOS[,yrs,],2:3,sum)/apply(dat$S[-1,yrs,],2:3,sum))

  pHOS_quants<-
    data.frame(
      apply(apply(sim_pHOS,c(1,3),quantile,qtiles),1:2,mean))
  #
  # Mean_pHOS_quants<-
  #   data.frame(cbind(
  #     apply(apply(sim_pHOS,c(1,3),mean),1,quantile,qtiles),
  #     Total=quantile(apply(sim_pHOS_tot,2,mean),qtiles)))

  ##pNOB
  sim_pNOB<-dat$pNOB[,yrs,]#dat$NOB[,yrs,]/(dat$HOB[-1,yrs,]+dat$NOB[,yrs,])
  # sim_pNOB_tot<-apply(dat$NOB[,yrs,],2:3,sum)/apply((dat$HOB[-1,yrs,]+dat$NOB[,yrs,]),2:3,sum)

  pNOB_quants<-   data.frame(
    apply(apply(sim_pNOB,c(1,3),quantile,qtiles),1:2,mean))

  # Mean_pNOB_quants<-
  #   data.frame(cbind(
  #     apply(apply(sim_pNOB,c(1,3),mean),1,quantile,qtiles),
  #     Total=quantile(apply(sim_pNOB_tot,2,mean),qtiles)))

  ##PNI
  sim_PNI<-dat$PNI[,yrs+8,] #sim_pNOB/(sim_pNOB+sim_pHOS)
  # sim_PNI_tot<-sim_pNOB_tot/(sim_pNOB_tot+sim_pHOS_tot)

  PNI_quants<-
    data.frame(
      apply(apply(sim_PNI,c(1,3),quantile,qtiles),1:2,mean))

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






   rbind((pHOS_quants |> t()|>
    `colnames<-`(c("min","LQI","med","UQI","max")) |>
    data.frame() |>
    rownames_to_column(rnames) |>
    mutate( HCR=HCR_name,metric="pHOS")),

   (pNOB_quants |> t()|>
     `colnames<-`(c("min","LQI","med","UQI","max")) |>
     data.frame() |>
     rownames_to_column(rnames) |>
     mutate( HCR=HCR_name,metric="pNOB")),

   (PNI_quants |> t()|>
      `colnames<-`(c("min","LQI","med","UQI","max")) |>
      data.frame() |>
      rownames_to_column(rnames) |>
      mutate( HCR=HCR_name,metric="PNI")))


}







plot_hatchery_quants<-function(hatchery_quants){

  # hatchery_quants<-do.call(rbind,lapply(names(sim_list),function(x) hatchery_quants_fun(sim_list[[x]],rnames="River",HCR_name=x)))|>
  #   mutate(River=fct_relevel(River,c("Wenatchee","Methow","Okanogan")),
  #          HCR=fct_relevel(HCR,names(sim_list)))


  hatchery_quants|>
     ggplot(aes(x = River, ymin = `min`, lower = `LQI`, middle = `med`, upper = `UQI`, ymax = `max`,fill=HCR))+geom_boxplot(stat="identity")+ylim(0,1)+scale_fill_brewer(palette="Dark2")+facet_wrap(~factor(metric,levels=c("pHOS","pNOB","PNI")))+ylab("Proportion")+geom_hline(data=tibble(yint=.67,metric="PNI"),aes(yintercept=yint),linetype="dashed",color="red",linewidth=1)+theme_gray(base_size = 16)+theme(axis.title.x=element_blank(),legend.position = "top")

}


hatchery_surplus_quants<-function(sim,yrs=7:31,qtiles=c(.025,.25,.5,.75,.975),HCR_name="Current"){
  surplus<-sim$escapement[1,yrs,] -sim$HOB[,yrs,] |> apply(2:3,sum)
 quants<-apply(apply(surplus,2,quantile,qtiles),1,mean)

 quants |> t() |> data.frame() |>
   `colnames<-`(c("min","LQI","med","UQI","max"))  |>
   mutate(HCR=HCR_name)
}

plot_hatchery_surplus<-function(surplus_quants){
  surplus_quants |> ggplot(aes(x = 1, ymin = `min`, lower = `LQI`, middle = `med`, upper = `UQI`, ymax = `max`,fill=HCR))+geom_boxplot(stat="identity")+ylab(ylab)+xlab("")+scale_fill_brewer(palette="Dark2")+theme_gray(base_size = 16)+theme(axis.ticks.x=element_blank(),axis.text.x = element_blank())+
    scale_y_continuous(labels = scales::unit_format(suffix="K",scale = 1e-3))+ylab("Hatchery surplus")
}
