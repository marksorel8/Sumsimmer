



sim_PFMC<-function(RMRS,
                   pfmc_err,
                    PFMC_ave_prop=internal_data$PFMC_ave_prop
                    ){

  exp(PFMC_ave_prop*log(RMRS)+pfmc_err)
}



sim_in_river<-function(allowed_Treaty_ER,
                       allowed_NT_ER,
                       in_river_err,
                       pfmc_AEQ,
                       RMRS,
                       coefs=internal_data$in_river_coefs,
                       URR , #release rate of unmarked fish handled in non-treaty fisheries
                       release_mort_rate = 0.15, #post release mortality rate
                       mark_rate # proportion of RMRS that is marked
                       ){




  tot_run_size<-RMRS+pfmc_AEQ # used in harvest control rule

  allowed_Treaty_HR<-(allowed_Treaty_ER*tot_run_size)/(RMRS) #convert ER to HR

  allowed_NT_HR<-
  #subtract PFMC, make sure positive and convert from exploitation rate to harvest rate (i.e. denominator from RMRM+ PFMC to just RMRS)
  (max(allowed_NT_ER-(pfmc_AEQ/(tot_run_size)),.0001)*tot_run_size)/(RMRS)




  #Segmented model of expected actual total harvest mortality as a funciton of allowed).
  #Treaty
  Treaty<- plogis(coefs[2]+ifelse(qlogis(allowed_Treaty_HR)>coefs[1],qlogis(allowed_Treaty_HR)-coefs[1],0)+in_river_err[1])

  #Non-treaty
  NT<-plogis(coefs[4]+ifelse(qlogis(allowed_NT_HR)>coefs[3],qlogis(allowed_NT_HR)-coefs[3],0)+in_river_err[2])

#reduce if culative harvest greater than 99% because just not realists
  while((Treaty+NT)>.99){
    Treaty<-Treaty*.9
    NT<-NT*.9
  }

  #   NT<- coefs[1]+NT_allowed_in_river*coefs[3]+ifelse(NT_allowed_in_river>coefs[5],(NT_allowed_in_river-coefs[5])*coefs[4],0)+(in_river_err[1])
  # NT<-max(NT,0)  #going with truncated normal errors because I would foresee management error in terms of numbers of fish being relatively constant across allowed harvest rates.
  # NT<-min(NT,RMRS*.45)
  #
  # #Treaty
  # Treaty<- coefs[1]+coefs[2]+allowed_Treaty*coefs[3]+ifelse(allowed_Treaty>coefs[5],(allowed_Treaty-coefs[5])*coefs[4],0)+(in_river_err[2])
  # Treaty<-max(Treaty,0)
  # Treaty<-min(Treaty,RMRS*.45)

  # working out marked and unmarked stuff for non_treaty
NT_handle<-NT/(1-URR*(1-release_mort_rate)*(1-mark_rate))

#reduce if culative harvest greater than 90% because just not realists


MHR=NT_handle #marked harvest rate
UHR=NT_handle*(1-URR*(1-release_mort_rate)) #unmarked harvest rate

#reduce marked harvest rate if cumulative harvest greater than 90% because just not realists
while((Treaty+MHR)>.99){
  MHR<-MHR*.9
  Treaty<-Treaty*.9
}

  c(Treaty=Treaty,
    NT_marked=MHR,
    NT_unmarked=UHR)
}

# returns allowed treaty HR
allowed_ER<-function(Run_size,
                          tiers,
                          rates,
                          scalar,
                          offset,
                          share
){

  tiers2<-tiers
  tiers2[is.na(tiers2)]<-Inf
  tier_num<-which.max(tiers2>Run_size)

  ifelse(!is.na(rates[tier_num]),
             ifelse(rates[tier_num] > 0 & rates[tier_num]< 1,rates[tier_num],rates[tier_num]/Run_size)
             ,
             ((scalar[tier_num]*Run_size - offset[tier_num])*share[tier_num])/Run_size
  )

}


allowed_Treaty<-function(Run_size,
                         PFMC,
                         tiers=c(16000,36250,50000,Inf),
                         rates=c(.05,.1,NA,NA),
                         scalar=c(NA,NA,1,.75),
                         offset=c(NA,NA,29000,16500),
                         share = c(NA,NA,.5,.5)
                         ){

  tiers2<-tiers
  tiers2[is.na(tiers2)]<-Inf
  tier_num<-which.max(tiers2>Run_size)

  ER<-ifelse(!is.na(rates[tier_num]),
             ifelse(rates[tier_num] > 0 & rates[tier_num]< 1,rates[tier_num],rates[tier_num]/Run_size)
             ,
             ((scalar[tier_num]*Run_size - offset[tier_num])*share[tier_num])/Run_size
            )


 (ER*Run_size)/(Run_size-PFMC) #convert ER to HR

}

# returns allowed NT HR
allowed_NT<-function(Run_size,
                     PFMC,
                     tiers=c(5000,16000,29000,32000,36250,50001,Inf),
                     rates=c(100,200,.05,.06,.07,NA,NA),
                     scalar=c(rep(NA,5),1,.75),
                     offset=c(rep(NA,5),29000,16500),
                     share=c(rep(NA,5),.5,.5)

){
  tiers2<-tiers
  tiers2[is.na(tiers2)]<-Inf
  tier_num<-which.max(tiers2>Run_size)

  tot_ER<-ifelse(!is.na(rates[tier_num]),
             ifelse(rates[tier_num] > 0 & rates[tier_num]< 1,rates[tier_num],rates[tier_num]/Run_size)
             ,
             ((scalar[tier_num]*Run_size - offset[tier_num])*share[tier_num])/Run_size
  )
 #
 # tot_ER<- ifelse(Run_size<=tiers[1],rates[1]/Run_size,
 #         ifelse(Run_size<=tiers[2],rates[2]/Run_size,
 #                ifelse(Run_size<=tiers[3],rates[3],
 #                       ifelse(Run_size<=tiers[4],rates[4],
 #                              ifelse(Run_size<=tiers[5],rates[5],
 #                                     ifelse(Run_size<=tiers[6],((rates[6]*(Run_size-29000)))/Run_size,#total harvestable = run size -29000
 #                                            (rates[7]*((0.75 * (Run_size-50000)) + 21000))/Run_size
 #                                     ))))))

#subtract PFMC, make sure positive and convert from exploitation rate to harvest rate (i.e. denominator from RMRM+ PFMC to just RMRS)
 (max(tot_ER-(PFMC/Run_size),.001)*Run_size)/(Run_size-PFMC)


}
