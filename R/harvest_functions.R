



sim_PFMC<-function(RMRS,
                   pfmc_err,
                    PFMC_ave_prop=internal_data$PFMC_ave_prop
                    ){

  exp(PFMC_ave_prop*log(RMRS)+pfmc_err)
}



sim_in_river<-function(allowed_Treaty,
                       NT_allowed_in_river,
                       in_river_err,
                       coefs=internal_data$in_river_coefs,
                       URR = internal_data$URR, #release rate of unmarked fish handled in non-treaty fisheries
                       release_mort_rate = 0.15, #post release mortality rate
                       mark_rate, # proportion of RMRS that is marked
                       RMRS
                       ){

  #Segmented model of expected actual total harvest mortality as a funciton of allowed).
  #Non-treaty
  NT<-coefs[1]+NT_allowed_in_river*coefs[3]+ifelse(NT_allowed_in_river>coefs[5],(NT_allowed_in_river-coefs[5])*coefs[4],0)+(in_river_err[1])
  NT<-max(NT,0)  #going with truncated normal errors because I would foresee management error in terms of numbers of fish being relatively constant across allowed harvest rates.

  #Treaty
  Treaty<- coefs[1]+coefs[2]+allowed_Treaty*coefs[3]+ifelse(allowed_Treaty>coefs[5],(allowed_Treaty-coefs[5])*coefs[4],0)+(in_river_err[2])
  Treaty<-max(Treaty,0)

  # working out marked and unmarked stuff for non_treaty
NT_handle<-NT/(1-URR*(1-release_mort_rate)*(1-mark_rate))

MHR=NT_handle/RMRS #marked harvest rate
UHR=NT_handle*(1-URR*(1-release_mort_rate))/RMRS #unmarked harvest rate

  c(Treaty=Treaty/RMRS,
    NT_marked=MHR,
    NT_unmarked=UHR)
}


allowed_Treaty<-function(Run_size,
                         tiers=c(16000,36250,50000),
                         rates=c(.05,.1,.5,.5)){
  ifelse(Run_size<=tiers[1],rates[1],
         ifelse(Run_size<=tiers[2],rates[2],
                ifelse(Run_size<=tiers[3],((rates[3]*(Run_size-29000))),#total harvestable = run size -29000
                       (rates[4]*((0.75 * (Run_size-50000)) + 21000))
                )))
}


allowed_NT<-function(Run_size,
                     PFMC,
                     tiers=c(5000,16000,29000,32000,36250,50001),
                     rates=c(100,200,.05,.06,.07,.5,.5)

){


  ifelse(Run_size<=tiers[1],rates[1],
         ifelse(Run_size<=tiers[2],rates[2],
                ifelse(Run_size<=tiers[3],rates[3]*Run_size,
                       ifelse(Run_size<=tiers[4],rates[4]*Run_size,
                              ifelse(Run_size<=tiers[5],rates[5]*Run_size,
                                     ifelse(Run_size<=tiers[6],((rates[6]*(Run_size-29000))),#total harvestable = run size -29000
                                            (rates[7]*((0.75 * (Run_size-50000)) + 21000))
                                     ))))))-PFMC

}
