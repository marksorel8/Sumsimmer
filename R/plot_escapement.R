

esc_traj_data_fun<-function(sim,yrs=7:31){
 list(ave= sim$escapement[,yrs,] |> apply(1:2,quantile,c(.025,.25,.5,.75,.975)) |> array2DF() |> `colnames<-`(c("quant","population_name","year","NO_Return")) |> dplyr::mutate(name="Escapement",year=as.numeric(year))|> tidyr::pivot_wider(names_from = quant,values_from = NO_Return) |> dplyr::rename(NO_Return=`50%`),

      rand=(sim$escapement[,yrs,15] |> array2DF()|> `colnames<-`(c("population_name","year","NO_Return"))|> dplyr::mutate(name="Escapement",year=as.numeric(year))))
}



plot_esc_trajectory<-function(esc_traj_dat,esc_dat=internal_data$esc_dat ){

  #plot
  esc_dat|> filter(between(year,1998,2023)) |>  #dplyr::select(year,population_name ,Spawn=NOS,Broodstock=NOBroodStockRemoved) %>%
   # tidyr::drop_na() |> tidyr::pivot_longer(c(Spawn,Broodstock),values_to = "NO_Return") %>%
  ggplot(aes(x=year))+geom_bar(aes(y=Escapement),stat="identity")+facet_wrap(~population_name,scales="free")+ylab("Escapement")+geom_ribbon(data=esc_traj_dat$ave,aes(x=year,ymin=`2.5%`,ymax=`97.5%`),fill="grey",)+geom_line(data=esc_traj_dat$ave,aes(x=year,y=NO_Return),lwd=2)+geom_line(data=esc_traj_dat$rand,aes(x=year,y=NO_Return) )+theme_gray(base_size = 16)+theme(legend.position = "top",axis.title.x = element_blank())+
    scale_y_continuous(labels = scales::unit_format(suffix="K",scale = 1e-3))#+scale_fill_brewer(palette="Dark2")

}


# NOS_quants<-do.call(rbind,lapply(names(sim_list),function(x) ave_quants(sim_list[[x]]$NOS,rnames="River",HCR_name=x))) |>
#   mutate(River=fct_relevel(River,c("Wenatchee","Methow","Okanogan","Total")),
#          HCR=fct_relevel(HCR,names(sim_list)))



plot_spawner_quants<-function(NOS_quants,ylab){
  NOS_quants |>
    ggplot(aes(x = River, ymin = `min`, lower = `LQI`, middle = `med`, upper = `UQI`, ymax = `max`,fill=HCR))+geom_boxplot(stat="identity")+ylab(ylab)+xlab("")+facet_wrap(~factor(River,levels=c("Wenatchee","Methow","Okanogan","Total")),scales="free",nrow=1)+scale_fill_brewer(palette="Dark2")+theme_gray(base_size = 16)+theme(axis.ticks.x=element_blank(),axis.text.x = element_blank())+
    scale_y_continuous(labels = scales::unit_format(suffix="K",scale = 1e-3))+theme(legend.position = "top")#+geom_hline(data=tibble(yintercept=c(20000,12143),Population=rep("Total",2),"Escapement\ngoal"=c("USvOR","PST")),aes(yintercept = yintercept,color=`Escapement\ngoal`),linetype = "dashed",lwd=1.5)+scale_color_brewer(palette="Paired")


    # ggplot(aes(x = River, ymin = `min`, lower = `LQI`, middle = `med`, upper = `UQI`, ymax = `max`,fill=HCR))+geom_boxplot(stat="identity")+ylab("Natural-origin spawners")+scale_fill_brewer(palette="Dark2")#+geom_hline(yintercept=50,lty=2)

}


# sim_list<-list("a"=sim)

# NOE_quants<-do.call(rbind,lapply(names(sim_list),function(x) ave_quants(sim_list[[x]]$escapement,rnames="Population",HCR_name=x))) |>
#   mutate(Population=fct_relevel(Population,c("Hatchery","Wenatchee","Methow","Okanogan","Total")),
#          HCR=fct_relevel(HCR,names(sim_list)))



plot_NOE_quants<-function(NOE_quants,nrw=1){
  NOE_quants |>
    ggplot(aes(x = Population, ymin = `min`, lower = `LQI`, middle = `med`, upper = `UQI`, ymax = `max`,fill=HCR))+geom_boxplot(stat="identity")+ylab("Escapement")+xlab("")+geom_hline(data=tibble(yintercept=c(20000,12143),Population=rep("Total",2),"Escapement\ngoal"=c("2018 USvOR","2019 PST & PFMC")),aes(yintercept = yintercept,color=`Escapement\ngoal`),linetype = "dashed",lwd=1.5)+facet_wrap(~factor(Population,levels=c("Wenatchee","Methow","Okanogan","Total_natural","Hatchery","Total")),scales="free",nrow=nrw)+scale_color_brewer(palette="Paired")+scale_fill_brewer(palette="Dark2")+theme_gray(base_size = 16)+theme(axis.ticks.x=element_blank(),axis.text.x = element_blank(),legend.position = "top")+guides(fill=guide_legend(nrow=2,byrow=TRUE),color=guide_legend(nrow=2,byrow=TRUE))+
    scale_y_continuous(labels = scales::unit_format(suffix="K",scale = 1e-3))#+geom_hline(yintercept=50,lty=2)

}



plot_MAT<-function(MAT){
  MAT |>  mutate(Population = fct_relevel(Population,c("Hatchery","Wenatchee","Methow","Okanogan","NO_total"))
  )  |>   # Exclude the baseline level itself
    ggplot(aes(x=Population,y=p_MA))+scale_fill_brewer(palette="Dark2")+geom_bar(aes(fill = HCR),stat = "identity",position = "dodge")+ylab("Prob > min abund")+theme_gray(base_size = 16)+theme(axis.title.x = element_blank(),legend.position = "top")
}



plot_NOE_ratios<-function(NOE_quants,no_harv="No harvest"){
# Calculate ratios
  NOE_quants %>%
  group_by(Population) %>%
  mutate(
    med= med/med[HCR==no_harv]) |>
  ungroup() |>
    mutate(Population = fct_relevel(Population,c("Hatchery","Wenatchee","Methow","Okanogan","Total"))
  )  |>   # Exclude the baseline level itself
  ggplot(aes(x=Population,y=med))+scale_fill_brewer(palette="Dark2")+geom_bar(aes(fill = HCR),stat = "identity",position = "dodge")+ylab("Escapement relative to no fishing")+theme_gray(base_size = 16)+theme(axis.title.x = element_blank(),legend.position = "top")
}


plot_pop_quants<-function(quants,lab){
  quants |>
    ggplot(aes(x = Population, ymin = `min`, lower = `LQI`, middle = `med`, upper = `UQI`, ymax = `max`,fill=HCR))+geom_boxplot(stat="identity")+ylab(lab)+xlab("")+facet_wrap(~factor(Population,levels=c("Hatchery","Wenatchee","Methow","Okanogan","Total")),scales="free",nrow=1)+scale_fill_brewer(palette="Dark2")+theme_gray(base_size = 16)+theme(axis.ticks.x=element_blank(),axis.text.x = element_blank(),legend.position = "top")+
    scale_y_continuous(labels = scales::unit_format(suffix="K",scale = 1e-3))
}


# pQET_fun<-function(QET=50,)
