harvest_quants_fun<-function(dat,HCR_name="Current",fun=mean,qtiles=c(.025,.25,.5,.75,.975),yrs=7:31,...){
  data.frame(rbind(
    apply(apply(apply(dat$terminal_NT[,yrs,],2:3,sum),2,quantile,qtiles,...),1,fun), # sum across populations, quantile across years, geometrtic mean across simulations
    apply(apply(apply(dat$terminal_NT[,yrs,],2:3,sum) + dat$PFMC[yrs,],2,quantile,qtiles,...),1,fun),
    apply(apply(apply(dat$terminal_treaty[,yrs,],2:3,sum),2,quantile,qtiles,...),1,fun)
  )) |>
    `colnames<-`(c("min","LQI","med","UQI","max")) |>
    mutate(Sector=c("NT_in_river","NT_plus_PFMC","Treaty"),
           HCR=HCR_name)
}




plot_harvest_quants<-function(harvest_quants,guide_rows=2,colors_vec){

  harvest_quants |> filter(HCR!="No harvest") |>
    mutate(Sector=ifelse(Sector=="NT_in_river","Non-treaty in-river",Sector)) |>
    ggplot(aes(x = Sector, ymin = `min`, lower = `LQI`, middle = `med`, upper = `UQI`, ymax = `max`,fill=HCR))+geom_boxplot(stat="identity")+ylab("Harvest")+scale_fill_manual(values  =colors_vec)+theme_gray(base_size = 16)+theme(axis.title.x = element_blank(),legend.key.height = unit(1, 'cm'),legend.key.width  = unit(1, 'cm'),legend.title = element_blank())+guides(fill=guide_legend(nrow=guide_rows,byrow=TRUE))+
    scale_y_continuous(labels = scales::unit_format(suffix="K",scale = 1e-3))+theme(legend.position = "top")

}



mean_harvest_quants_fun<-function(dat,HCR_name="Current",fun=mean,qtiles=c(0,.25,.5,.75,1),yrs=7:31,...){
  data.frame(rbind(
    quantile(apply(apply(dat$terminal_NT[,yrs,],2:3,sum),2,mean),qtiles), # sum across populations, quantile across years, geometrtic mean across simulations
    quantile(apply(apply(dat$terminal_treaty[,yrs,],2:3,sum),2,mean),qtiles)
  )) |>
    `colnames<-`(c("min","LQI","med","UQI","max")) |>
    mutate(Sector=c("NT_in_river","Treaty"),
           HCR=HCR_name)
}



plot_mean_harvest_quants<-function(mean_harvest_quants,guide_rows=2,colors_vec){

  mean_harvest_quants |> filter(HCR!="No harvest") |>
    mutate(Sector=ifelse(Sector=="NT_in_river","Non-treaty in-river",Sector)) |>
    ggplot(aes(x = Sector, ymin = `min`, lower = `LQI`, middle = `med`, upper = `UQI`, ymax = `max`,fill=HCR))+geom_boxplot(stat="identity")+ylab("Mean harvest")+scale_fill_manual(values=colors_vec)+theme_gray(base_size = 16)+theme(axis.title.x = element_blank())+guides(fill=guide_legend(nrow=guide_rows,byrow=TRUE))+
    scale_y_continuous(labels = scales::unit_format(suffix="K",scale = 1e-3))+theme(legend.position = "top",legend.key.height = unit(1, 'cm'),legend.key.width  = unit(1, 'cm'),legend.title = element_blank())

}



low_harvest_quants_fun<-function(dat,HCR_name="Current",fun=mean,nyrs=5,qtiles=c(0,.25,.5,.75,1),yrs=7:31,...){

 low_NT<- apply( dat$terminal_NT[,yrs,],2:3,sum)|>
   apply(2,sort) |>
   head(nyrs)|>
   apply(2,fun) |>
   quantile(qtiles)

 low_T<- apply(dat$terminal_treaty[,yrs,],2:3,sum) |>
   apply(2,sort) |>
   head(nyrs) |>
   apply(2,fun) |>
   quantile(qtiles)




  data.frame(rbind(
    low_NT, # sum across populations, quantile across years, geometrtic mean across simulations
    low_T
  )) |>
    `colnames<-`(c("min","LQI","med","UQI","max")) |>
    mutate(Sector=c("NT_in_river","Treaty"),
           HCR=HCR_name)
}



plot_low_harvest_quants<-function(low_harvest_quants,guide_rows=2,colors_vec){

  low_harvest_quants |> filter(HCR!="No harvest") |>
    mutate(Sector=ifelse(Sector=="NT_in_river","Non-treaty in-river",Sector)) |>
    ggplot(aes(x = Sector, ymin = `min`, lower = `LQI`, middle = `med`, upper = `UQI`, ymax = `max`,fill=HCR))+geom_boxplot(stat="identity")+ylab("Mean harvest in lowest 20% of years")+scale_fill_manual(values  =colors_vec)+theme_gray(base_size = 16)+theme(axis.title.x = element_blank())+guides(fill=guide_legend(nrow=guide_rows,byrow=TRUE))+
    scale_y_continuous(labels = scales::unit_format(suffix="K",scale = 1e-3))+theme(legend.position = "top",legend.key.height = unit(1, 'cm'),legend.key.width  = unit(1, 'cm'),legend.title = element_blank())

}


# p1<-plot_all_fun(sim_list)


seq_HCR<-function(index="total",
                  pfmc_cutoff=29000,
                  treaty_tiers = c(16000,36250,50000,Inf),
                  treaty_rates = c(.05,.1,NA,NA),
                  treaty_scalar = c(NA,NA,1,.75),
                  treaty_offset = c(NA,NA,29000,16500),
                  treaty_share = c(NA,NA,.5,.5),
                  NT_tiers = c(5000,16000,29000,32000,36250,50001,Inf),
                  NT_rates = c(100,200,.05,.06,.07,NA,NA),
                  NT_scalar = c(rep(NA,5),1,.75),
                  NT_offset = c(rep(NA,5),29000,16500),
                  NT_share = c(rep(NA,5),.5,.5),
                  n = 200,
                  max_RMRS=NULL
){

  sum_tab<-data.frame(
    index = index,
    pfmc_cutoff =pfmc_cutoff,
    treaty_tiers = c(treaty_tiers,rep(NA,20-length(treaty_tiers))),
    treaty_rates = c(treaty_rates,rep(NA,20-length(treaty_rates))),
    treaty_scalar = c(treaty_scalar,rep(NA,20-length(treaty_scalar))),
    treaty_offset = c(treaty_offset,rep(NA,20-length(treaty_offset))),
    treaty_share = c(treaty_share,rep(NA,20-length(treaty_share))),
    NT_tiers = c(NT_tiers,rep(NA,20-length(NT_tiers))),
    NT_rates = c(NT_rates,rep(NA,20-length(NT_rates))),
    NT_scalar = c(NT_scalar,rep(NA,20-length(NT_scalar))),
    NT_offset = c(NT_offset,rep(NA,20-length(NT_offset))),
    NT_share = c(NT_share,rep(NA,20-length(NT_share)))
  )



  tryCatch({
    check_HCR(treaty_tiers,
              treaty_rates,
              treaty_scalar,
              treaty_offset,
              treaty_share,
              "Treaty")



    check_HCR(NT_tiers,
              NT_rates,
              NT_scalar,
              NT_offset,
              NT_share,
              "Non-treaty")


    if(is.null(max_RMRS)){
    if(index=="total"){
      RMRS<-seq(2000,125000,length.out=n)
    }else{
      RMRS<-seq(1000,50000,length.out=n)
    }
    }else{
      RMRS<-seq(1000,max_RMRS,length.out=n)
    }

    PFMC<-sim_PFMC(RMRS,pfmc_err=0)
    PFMC_2<-ifelse((RMRS+PFMC)<pfmc_cutoff,0,PFMC)
    Treaty<-numeric(n)

    if(index=="total"){
      AI<-RMRS+PFMC
    }else{
      if(index=="wild"){
        AI<-RMRS+PFMC#+wild_PFMC[y,i]
      }
    }


    for(i in 1:n){
      allowed<-allowed_ER(AI[i],
                          treaty_tiers,
                          treaty_rates,
                          treaty_scalar,
                          treaty_offset,
                          treaty_share
      )
      Treaty[i]<-(allowed*(RMRS[i]+PFMC[i]))/(AI[i])
    }

    NT<-numeric(n)
    NT_w_PFMC<-numeric(n)
    for(i in 1:n){
      allowed<-allowed_ER(AI[i],
                          NT_tiers,
                          NT_rates,
                          NT_scalar,
                          NT_offset,
                          NT_share)

      NT_w_PFMC[i]<-((allowed*AI[i]))/(AI[i])

      NT[i]<-((allowed*(AI[i]))-PFMC_2[i])/(AI[i])
    }


    list(Treaty = Treaty,
         NT = NT,
         NT_w_PFMC = NT_w_PFMC,
         RMRS = RMRS,
         PFMC = PFMC_2,
         AI=AI,
         sum_tab=sum_tab
    )

  }, error=function(e){
    return(e)
  })
}


plot_HCR<-function(HR_seqs,
                   Total_NT=FALSE){
  with(HR_seqs,{
    if(Total_NT){
      plot(AI,NT_w_PFMC,type="l",lwd=2,ylab="Exploitation rate (includes PFMC for non-treaty)",ylim=c(min(c(Treaty,NT)-.025),max(c(Treaty+NT_w_PFMC)*1.15)),xlab="Ocean Abundance")
      NT_out<-NT_w_PFMC
      abline(0,0,lty=2)
    }else{
      plot(AI,NT,type="l",lwd=2,ylab="Exploitation (excludes PFMC for non-treaty)",ylim=c(min(c(Treaty,NT)-.025),max(c(Treaty+NT)*1.15)),xlab="Ocean Abundance")
      NT_out<-NT
      abline(0,0,lty=2)
    }
    points(AI,Treaty,type="l",col="firebrick4",lwd=2)
    points(AI,Treaty+NT_out,type="l",col="darkblue",lwd=2)

    legend("topleft",c("Treaty","Non-treaty","Total"),lty=1,lwd=2,col=c("firebrick4","black","darkblue"))
  })
}

plot_HCR_compare<-function(seq_list,
                           which_type="Total",
                           colors=NULL,
                           line_type=1,
                           line_width=1.85,
                           poly_df){

  tbl <- as_tibble(do.call(rbind, lapply(seq_list,as.data.frame)))|>
    mutate(`Harvest rule` = rep(names(seq_list),each =length(seq_list[[1]][[1]]))) |>
    mutate(Index =ifelse(sum_tab.index=="total","Total","Wild"))

  tbl2 <- tbl |> select(`Harvest rule`,
                        index = sum_tab.index,
                        pfmc_cutoff = sum_tab.pfmc_cutoff,
                        tiers = sum_tab.NT_tiers,
                        rates = sum_tab.NT_rates,
                        scalar = sum_tab.NT_scalar,
                        offset = sum_tab.NT_offset,
                        share = sum_tab.NT_share
  )|>
    distinct() |>
    filter(!(is.na(tiers)&
             is.na(rates)&
             is.na(scalar)&
             is.na(offset)&
             is.na(share)))

  tbl3 <- tbl |> select(`Harvest rule`,
                        index = sum_tab.index,
                        pfmc_cutoff = sum_tab.pfmc_cutoff,
                        tiers = sum_tab.treaty_tiers,
                        rates = sum_tab.treaty_rates,
                        scalar = sum_tab.treaty_scalar,
                        offset = sum_tab.treaty_offset,
                        share = sum_tab.treaty_share,
                        NT_tiers = sum_tab.NT_tiers
  )|>
    distinct()|>
    filter(!(is.na(tiers)&
               is.na(rates)&
               is.na(scalar)&
               is.na(offset)&
               is.na(share)))



  plot_dat<-tbl %>%
    select(`Harvest rule`, everything())%>%
    mutate(total_in_river=Treaty+NT,
           total_w_PFMC=Treaty+NT_w_PFMC,
           PFMC_ER=PFMC/AI) %>%
    pivot_longer(cols=c(Treaty,
                        NT,
                        NT_w_PFMC,
                        total_in_river,
                        total_w_PFMC,
                        PFMC_ER),
                 names_to = "sector",
                 values_to="Harvest rate"
    ) |>
    mutate(sector=case_when(sector=="Treaty"~"Treaty",
                            sector=="NT"~"Non-treaty in-river",
                            sector=="NT_w_PFMC"~"Non-treaty",
                            sector=="total_in_river"~"5) Total in-river",
                            sector=="total_w_PFMC"~"6) Total w/ PFMC",
                            sector=="PFMC_ER"~"4) just PFMC"),
           `Harvest rule`=fct_relevel(`Harvest rule`,names(seq_list))) |>
    ungroup() |>

#   [1] "#1B9E77" "#D95F02" "#7570B3" "#E7298A" "#66A61E"
# [6] "#E6AB02" "#A6761D" "#666666"

    filter(sector%in%c("Treaty","Non-treaty"))

  if(is.null(colors)){
    colors<-c("#7BAEA0", "#386276", "#3A4332", "#7A7D6F", "#D9B96E","#BED4F0")[c(-1,-which((plot_dat |> group_by(`Harvest rule`) |> select(Index) |> distinct() |> pull(Index))!={{which_type}}))]
}
    plot<-plot_dat |>filter(`Harvest rule`!="No harvest",Index=={{which_type}}) |> #mutate(`Harvest rule`=paste0(`Harvest rule`," ",sector)) |>
      ggplot() +
  geom_polygon(data=poly_df,aes(x=AI,y=green_y),fill="#009E73",alpha=0.5)+
  geom_polygon(data=poly_df,aes(x=AI,y=red_y),fill="#CC79A7",alpha=0.5)+
      geom_line(aes(x=AI,y=`Harvest rate`*100,color=`Harvest rule`,linetype = `Harvest rule`),size=line_width,alpha=.8)+  scale_linetype_manual(values = c(1,1,2)) + scale_size_manual(values = c(1.5,1.5,1.5,1.5))  +
      facet_wrap(~sector)+
      scale_color_manual(values  =colors)+xlab(paste(which_type,"SUS Ocean Index"))+geom_hline(aes(yintercept=0),linetype = 2)+ylab("Allowed harvest %")+
    scale_x_continuous(labels = scales::unit_format(suffix="K",scale = 1e-3))+guides(colour = guide_legend(override.aes = list(lwd=c(2.5,1.5,1))))+
      theme_gray(base_size = 18)+theme(legend.key.width = unit(2.5, 'cm'),legend.position = "top",legend.title = element_blank()) #+ guides(linetype = guide_legend(override.aes = list(size = 2)))
    # guides(color=guide_legend(nrow=2,byrow=TRUE))

  list(tbl2=tbl2,
       tbl3=tbl3,
       plot_dat=plot_dat,
       plot=plot)
}

#plot_HCR()

harv_traj_dat_fun<-function(sim,yrs=7:31){
  list(ave= (apply(sim$terminal_NT[,yrs,],2:3,sum) |>  apply(1,quantile,c(.025,.25,.5,.75,.975)) |> t() |> tibble::as_tibble() |> dplyr::mutate(Sector="Non-treaty",year=2024:2048) |>
               dplyr::bind_rows(
                 apply(sim$terminal_treaty[,yrs,],2:3,sum) |>  apply(1,quantile,c(.025,.25,.5,.75,.975)) |> t() |> tibble::as_tibble() |> dplyr::mutate(Sector="Treaty",year=2024:2048)
               ) |> dplyr::rename(In_river_harvest=`50%`)),
       rand=tibble::tibble(In_river_harvest=c(apply(sim$terminal_NT[,yrs,],2:3,sum)[,15],
                                              apply(sim$terminal_treaty[,yrs,],2:3,sum)[,15]),
                           year=rep(2024:2048,time=2),
                           Sector=rep(c("Non-treaty","Treaty"),each=25))
  )
}

plot_harvest_trajectory<-function(harv_traj_dat){

  internal_data$pfmc_morts_2|> dplyr::mutate(Sector=ifelse(Sector=="NT","Non-treaty",Sector)) |> dplyr::filter(Year<=2022) |> ggplot(aes(x=Year,y=In_river_harvest))+geom_col()+facet_wrap(~Sector)+ylab("In-river harvest")+
    geom_ribbon(data=harv_traj_dat$ave,aes(x=year,ymin=`2.5%`,ymax=`97.5%`),fill="grey")+
    geom_line(data=harv_traj_dat$ave,aes(x=year,y=In_river_harvest),lwd=1.7)+
    geom_line(data=harv_traj_dat$rand,aes(x=year,y=In_river_harvest))+scale_fill_brewer(palette="Dark2")+theme_gray(base_size = 16)+theme(axis.title.x = element_blank())+
    scale_y_continuous(labels = scales::unit_format(suffix="K",scale = 1e-3))
}


# plot_esc_trajectory(sim)
# plot_harvest_trajectory(sim2)


plot_past_harvest<-function(hcr_list_simple,plot_all=TRUE,colors_vec){

  df<-Sumsimmer:::internal_data$plotting_past_year_df |>  expand_grid(HCR=names(hcr_list_simple)) |>
    mutate(Treaty=NA,`Non treaty`=NA,`NT - 8%`=NA,
           HCR=fct_relevel(HCR,names(hcr_list_simple)))

  for( i in 1:nrow(df)){

    AI<-ifelse(hcr_list_simple[[df$HCR[i]]]$index=="total",df$`PFMC Ocean Abundance`[i], df$wild_AI[i])

    df[i,"Treaty"]<-allowed_ER(AI,
                                  hcr_list_simple[[df$HCR[i]]]$treaty_tiers,
                                  hcr_list_simple[[df$HCR[i]]]$treaty_rates,
                                  hcr_list_simple[[df$HCR[i]]]$treaty_scalar,
                                  hcr_list_simple[[df$HCR[i]]]$treaty_offset,
                                  hcr_list_simple[[df$HCR[i]]]$treaty_share
    )



    df[i,"Non treaty"]<-allowed_ER(AI,
                              hcr_list_simple[[df$HCR[i]]]$NT_tiers,
                              hcr_list_simple[[df$HCR[i]]]$NT_rates,
                              hcr_list_simple[[df$HCR[i]]]$NT_scalar,
                              hcr_list_simple[[df$HCR[i]]]$NT_offset,
                              hcr_list_simple[[df$HCR[i]]]$NT_share
    )


    df[i,"NT minus 8%"]<-ifelse(AI<hcr_list_simple[[df$HCR[i]]]$pfmc_cutoff,df[i,"Non treaty"],df[i,"Non treaty"]-df$`PFMC NT Ocean Impacts`[i])


  }

# ER_plot<-  df |> pivot_longer(c(Treaty,`Non treaty`,`NT minus 8%`),names_to = "Sector", values_to = "ER") |>
#   mutate(Sector=fct_relevel(Sector,c("Treaty","Non treaty","NT minus 8%"))) |>
#
#     ggplot(aes(x=Year,y=ER*100,fill=HCR))+geom_bar(position = "dodge", stat = "identity")+ylab("Treaty")+theme(axis.title.x =element_blank(),text = element_text(size=16))+scale_fill_manual(values =RColorBrewer::brewer.pal(8,"Dark2")[c(-1)])+
#     theme(legend.position = "top")+facet_wrap(~Sector,strip.position="right",ncol = 1)+ylab("Allowed ER %")


  Treaty<-df |>
    ggplot(aes(x=Year,y=Treaty*100,fill=HCR))+geom_bar(position = "dodge", stat = "identity")+ylab("Treaty")+theme(axis.title.x =element_blank(),text = element_text(size=17))+scale_fill_manual(values =colors_vec)+
    theme(legend.position = "top",axis.title.x = element_blank(),axis.text.x = element_blank())

  NT<-df |>
    ggplot(aes(x=Year,y=`Non treaty`*100,fill=HCR))+geom_bar(position = "dodge", stat = "identity")+ylab("Non-treaty")+theme(axis.title.x =element_blank(),text = element_text(size=17))+scale_fill_manual(values =colors_vec)+
    theme(legend.position = "top",axis.title.x = element_blank(),axis.text.x = element_blank())


  NT_in_river<-df |>
    ggplot(aes(x=Year,y=`NT minus 8%`*100,fill=HCR))+geom_bar(position = "dodge", stat = "identity")+ylab("NT minus 8%")+theme(axis.title.x =element_blank(),text = element_text(size=17))+scale_fill_manual(values =colors_vec)+
    theme(legend.position = "top")


  df |>mutate(tot=Treaty+.02*(`Non treaty`-`PFMC NT Ocean Impacts`)+`PFMC NT Ocean Impacts`) |>
    ggplot(aes(x=Year,y=tot*100,fill=HCR))+geom_bar(position = "dodge", stat = "identity")+ylab("Wild harvest impacts below Snake\n(including SUS Ocean)")+theme(axis.title.x =element_blank(),text = element_text(size=14))+scale_fill_manual(values =colors_vec)+
    theme(legend.position = "top",legend.title = element_blank(),text=element_text(size=16))




  AI_plot<-df |> select(Year,`PFMC Ocean Abundance`,wild_AI) |> pivot_longer(c(`PFMC Ocean Abundance`,wild_AI),names_to = "Index",values_to = "value") |> distinct() |>
    ggplot(aes(x=Year,y=value/1000,fill=Index))+geom_bar(position = "dodge", stat = "identity")+ylab("Adults (1,000s)")+theme(axis.title.x =element_blank(),text = element_text(size=14))+scale_fill_manual(values =tail(colors_vec,2))+
    theme(legend.position = "top",legend.title = element_blank())

  # ggpubr::ggarrange(ER_plot,AI_plot,ncol=1,heights=c(1,.35),align="v")

if(plot_all){
 plot<- ggpubr::ggarrange(
    ggpubr::ggarrange(Treaty,NT,NT_in_river,ncol=1,common.legend = TRUE, legend = "top",align="v"),AI_plot,ncol=1,heights=c(1,.35))
}else{
  ER_plot<-df |>
    ggplot(aes(x=Year,y=Treaty*100,fill=HCR))+geom_bar(position = "dodge", stat = "identity")+ylab("Allowed ER %")+theme(axis.title.x =element_blank(),text = element_text(size=14))+scale_fill_manual(values =colors_vec)+
    theme(legend.position = "top",axis.title.x = element_blank(),axis.text.x = element_blank(),legend.title = element_blank())

 plot<- ggpubr::ggarrange(ER_plot,AI_plot,ncol=1,common.legend = FALSE, legend = "top",align="v",heights=c(1,.65))
}

  list(plot=plot,
       df=df)

}
