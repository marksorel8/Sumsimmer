# sim<-pop_sim()


CV<-function(x){(sd(x)/mean(x))*100}


geo_mean<-function(x){exp(mean(log(x)))}

ave_quants<-function(x,fun=geo_mean){
  data.frame(cbind(
    apply(apply(x[,yrs,],c(1,3),quantile),1:2,fun),
    Total=apply(apply(apply(x[,yrs,],2:3,sum),2,quantile),1,fun)))
}

quants_of_ave<-function(x,fun=geo_mean){
  data.frame(cbind(
    apply(apply(x[,yrs,],c(1,3),fun),1,quantile),
    Total=quantile(apply(apply(x[,yrs,],2:3,sum),2,fun))))
}



summarize_sim<-function(sim,yrs=7:31){
  library(tidyverse)
  pfmc_morts<-readxl::read_xlsx("data-raw/data/New Summer Chinook Reconstruction  101023.xlsx",sheet="TotalRunSize",skip=2,n_max = 45) |>
    filter(Year>=2008) |> rename(`PFMC NT Harvest Total`=`PFMC NT Ocean Impacts`)

  #Average RMRS

  #Harvest
  ##  across-year quantiles: min, 25%, median, 75%, max

  harvest_quants<-data.frame(rbind(
  apply(apply(apply(sim$terminal_NT[,yrs,],2:3,sum),2,quantile),1,geo_mean), # sum across populations, quantile across years, geometrtic mean across simulations
  apply(apply(apply(sim$terminal_NT[,yrs,],2:3,sum) + sim$PFMC[yrs,],2,quantile),1,geo_mean),
  apply(apply(apply(sim$terminal_treaty[,yrs,],2:3,sum),2,quantile),1,geo_mean)
  ))
  colnames(harvest_quants)<-c("min","LQI","med","UQI","max")
  harvest_quants$Sector<-c("NT_in_river","NT_plus_PFMC","Treaty")



  harvest_quants
  rbind(
  quantile(head(pfmc_morts$`Total In-river NT`,-1)),
  quantile(head(pfmc_morts$`Total NT`,-1)),
  quantile(head(pfmc_morts$`Total Treaty`,-1))
  )

  ## Across year geometric mean harvest
  average_harvest_quants<-data.frame(rbind(
    quantile(apply(apply(sim$terminal_NT[,yrs,],2:3,sum),2,geo_mean)), # sum across populations, geometrtic mean across simulations
    quantile(apply(apply(sim$terminal_NT[,yrs,],2:3,sum) + sim$PFMC[yrs,],2,geo_mean)),
    quantile(apply(apply(sim$terminal_treaty[,yrs,],2:3,sum),2,geo_mean))
  ))
  colnames(average_harvest_quants)<-c("min","LQI","med","UQI","max")
  average_harvest_quants$Sector<-c("NT_in_river","NT_plus_PFMC","Treaty")

  average_harvest_quants
  rbind(
    geo_mean(head(pfmc_morts$`Total In-river NT`,-1)),
    geo_mean(head(pfmc_morts$`Total NT`,-1)),
    geo_mean(head(pfmc_morts$`Total Treaty`,-1))
  )

  ## average ratio of treaty to non-treaty
  ### quantiles of across-year mean
  Sector_ratio<-data.frame(rbind(
  quantile(apply(apply(sim$terminal_treaty[,yrs,],2:3,sum)/apply(sim$terminal_NT[,yrs,],2:3,sum),2,mean)),
  quantile(apply(apply(sim$terminal_treaty[,yrs,],2:3,sum)/(apply(sim$terminal_NT[,yrs,],2:3,sum)+ sim$PFMC[yrs,]),2,mean))
  ))
  colnames(Sector_ratio)<-c("min","LQI","med","UQI","max")
  Sector_ratio$ratio<-c("Treaty:NT_in_river","Treaty:NT_Total")

  Sector_ratio

  rbind(
quantile(head(pfmc_morts$`Total Treaty`,-1)/head(pfmc_morts$`Total In-river NT`,-1)),
quantile(head(pfmc_morts$`Total Treaty`,-1)/head(pfmc_morts$`Total NT`,-1)))


    ## harvest variability (year to year)



  CV_harvest_quants<-data.frame(rbind(
    quantile(apply(apply(sim$terminal_NT[,yrs,],2:3,sum),2,CV)), # sum across populations, geometrtic mean across simulations
    quantile(apply(apply(sim$terminal_NT[,yrs,],2:3,sum) + sim$PFMC[yrs,],2,CV)),
    quantile(apply(apply(sim$terminal_treaty[,yrs,],2:3,sum),2,CV))
  ))
  colnames(CV_harvest_quants)<-c("min","LQI","med","UQI","max")
  CV_harvest_quants$Sector<-c("NT_in_river","NT_plus_PFMC","Treaty")

  CV_harvest_quants
  rbind(
    CV(head(pfmc_morts$`Total In-river NT`,-1)),
    CV(head(pfmc_morts$`Total NT`,-1)),
    CV(head(pfmc_morts$`Total Treaty`,-1))
  )


  #Conservation



  ## Average NO spawners
   NOS_quants<-ave_quants(sim$NOS)
   Geomean_NOS_quants<-quants_of_ave(sim$NOS)

  ## Average total spawners
   S_quants<-ave_quants(sim$S)
   Geomean_S_quants<-quants_of_ave(sim$S)

  ## Average pNOB
   sim_pNOB<-sim$NOB[,yrs,]/c("Methow"=122,"Okanogan" = 650, "Wenatchee" = 310)
   sim_pNOB_tot<-apply(sim$NOB[,yrs,],2:3,sum)/sum(c("Methow"=122,"Okanogan" = 650, "Wenatchee" = 310))

pNOB_quants<-   data.frame(cbind(
     apply(apply(sim_pNOB,c(1,3),quantile),1:2,mean),
     Total=apply(apply(sim_pNOB_tot,2,quantile),1,mean)))

Mean_pNOB_quants<-
  data.frame(cbind(
    apply(apply(sim_pNOB,c(1,3),mean),1,quantile),
    Total=quantile(apply(sim_pNOB_tot,2,mean))))


  ## Average pHOS
sim_pHOS<-1-(sim$NOS[,yrs,]/sim$S[-1,yrs,])
sim_pHOS_tot<-1-(apply(sim$NOS[,yrs,],2:3,sum)/apply(sim$S[-1,yrs,],2:3,sum))

   pHOS_quants<- data.frame(cbind(
     data.frame(cbind(
       apply(apply(sim_pHOS,c(1,3),mean),1,quantile),
       Total=quantile(apply(sim_pNOB_tot,2,mean))))

   Mean_pHOS_quants<-

  ## Average pNI
   S_quants<-ave_quants(sim$S)
   Geomean_S_quants<-quants_of_ave(sim$S)


}
