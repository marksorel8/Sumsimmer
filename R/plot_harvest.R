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




plot_harvest_quants<-function(harvest_quants){

  harvest_quants |>
    ggplot(aes(x = Sector, ymin = `min`, lower = `LQI`, middle = `med`, upper = `UQI`, ymax = `max`,fill=HCR))+geom_boxplot(stat="identity")+ylab("Harvest")+scale_fill_brewer(palette="Dark2")+theme_gray(base_size = 16)+theme(axis.title.x = element_blank())+guides(fill=guide_legend(nrow=2,byrow=TRUE))+
    scale_y_continuous(labels = scales::unit_format(suffix="K",scale = 1e-3))

}








# p1<-plot_all_fun(sim_list)


seq_HCR<-function(index="total",
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
                  n = 200
){
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

if(index=="total"){
    RMRS<-seq(2000,150000,length.out=n)
}else{
  RMRS<-seq(1000,50000,length.out=n)
}

    PFMC<-sim_PFMC(RMRS,pfmc_err=0)
    PFMC_2<-ifelse((RMRS+PFMC)>29000|index=="wild",PFMC,0)
    Treaty<-numeric(n)

    if(index=="total"){
      AI<-RMRS+PFMC
    }else{
      if(index=="wild"){
        AI<-RMRS#+wild_PFMC[y,i]
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
      Treaty[i]<-(allowed*(RMRS[i]+PFMC[i]))/(RMRS[i])
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

      NT_w_PFMC[i]<-((allowed*(RMRS[i]+PFMC_2[i])))/(RMRS[i])

      NT[i]<-((allowed*(RMRS[i]+PFMC_2[i]))-PFMC_2[i])/(RMRS[i])
    }

    list(Treaty = Treaty,
         NT = NT,
         NT_w_PFMC = NT_w_PFMC,
         RMRS = RMRS,
         PFMC = PFMC_2
         )

  }, error=function(e){
    return(e)
  })
}


plot_HCR<-function(HR_seqs,
                   Total_NT=FALSE){
  with(HR_seqs,{
    if(Total_NT){
      plot(RMRS,NT_w_PFMC,type="l",lwd=2,ylab="Harvest rate (includes PFMC for non-treaty)",ylim=c(min(c(Treaty,NT)-.025),max(c(Treaty+NT_w_PFMC)*1.15)),xlab="River mouth run size")
      NT_out<-NT_w_PFMC
      abline(0,0,lty=2)
    }else{
      plot(RMRS,NT,type="l",lwd=2,ylab="Harvest rate (excludes PFMC for non-treaty)",ylim=c(min(c(Treaty,NT)-.025),max(c(Treaty+NT)*1.15)),xlab="River mouth run size")
      NT_out<-NT
      abline(0,0,lty=2)
    }
    points(RMRS,Treaty,type="l",col="firebrick4",lwd=2)
    points(RMRS,Treaty+NT_out,type="l",col="darkblue",lwd=2)

    legend("topleft",c("Treaty","Non-treaty","Total"),lty=1,lwd=2,col=c("firebrick4","black","darkblue"))
  })
}

plot_HCR_compare<-function(seq_list){
  tbl <- as_tibble(do.call(rbind, lapply(seq_list, as.data.frame)))
  tbl |>
    mutate(`Harvest rule` = rep(names(seq_list),each=length(seq_list[[1]][[1]])))%>%
    select(`Harvest rule`, everything())%>%
    mutate(total_in_river=Treaty+NT,
           total_w_PFMC=Treaty+NT_w_PFMC,
           PFMC_HR=PFMC/RMRS) %>%
    pivot_longer(cols=c(Treaty,
                        NT,
                        NT_w_PFMC,
                        total_in_river,
                        total_w_PFMC,
                        PFMC_HR),
                 names_to = "sector",
                 values_to="Harvest rate"
    ) |>
    mutate(sector=case_when(sector=="Treaty"~"1) Treaty",
                            sector=="NT"~"2) Non-treaty in-river",
                            sector=="NT_w_PFMC"~"3) Non-treaty w/ PFMC",
                            sector=="total_in_river"~"5) Total in-river",
                            sector=="total_w_PFMC"~"6) Total w/ PFMC",
                            sector=="PFMC_HR"~"4) just PFMC"),
           `Harvest rule`=fct_relevel(`Harvest rule`,names(seq_list))) |>

    ggplot(aes(x=RMRS,y=`Harvest rate`,color=`Harvest rule`))+geom_line(lwd=2,alpha=.8)+facet_wrap(~sector)+scale_color_brewer(palette="Dark2")+xlab("River Mouth Run Size")+theme_gray(base_size = 16)+geom_hline(aes(yintercept=0),linetype = 2)+ylab("Allowed harvest rate")+
    scale_x_continuous(labels = scales::unit_format(suffix="K",scale = 1e-3))


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
    geom_line(data=harv_traj_dat$ave,aes(x=year,y=In_river_harvest),lwd=2)+
    geom_line(data=harv_traj_dat$rand,aes(x=year,y=In_river_harvest))+scale_fill_brewer(palette="Dark2")+theme_gray(base_size = 16)+theme(axis.title.x = element_blank())+
    scale_y_continuous(labels = scales::unit_format(suffix="K",scale = 1e-3))
}


# plot_esc_trajectory(sim)
# plot_harvest_trajectory(sim2)
