



sim_PFMC<-function(RMRS,
                    PFMC_ave_prop=PFMC_ave_prop,
                    sd_lar_pfmc=sd_lar_pfmc){

  exp(PFMC_ave_prop*log(RMRS)+(rnorm(1,0,sd_lar_pfmc)))
}



sim_in_river<-function(allowed,
                       PFMC,
                       coefs,
                       psi,
                       sd_lar_in_river){

  NT_allowed<-allowed-PFMC
  #Non-treaty
  NT<-coefs[1]+NT_allowed*coefs[3]+ifelse(NT_allowed>psi,NT_allowed*coefs[4],0)
  #Treaty
  Treaty<- coefs[1]+coefs[2]+allowed*coefs[3]+ifelse(NT_allowed>psi,allowed*coefs[4],0)

  #going with truncated normal errors because I would foresee management error in terms of numbers of fish being relatively constant across allowed harvest rates.
  c(min(NT+(rnorm(1,0,sd_lar_in_river)),0),
    min(Treaty+(rnorm(1,0,sd_lar_in_river))))
}


allowed_Treaty<-function(Run_size,
                         tiers=c(16000,36250,50000),
                         rates=c(.05,.1,.5,.5)){
  ifelse(Run_size<=tiers[1],rates[1],
         ifelse(Run_size<=tiers[2],rates[2],
                ifelse(Run_size<=tiers[3],((rates[3]*(Run_size-29000))/Run_size),#total harvestable = run size -29000
                       (rates[4]*((0.75 * (Run_size-50000)) + 21000))/Run_size
                )))
}


allowed_NT<-function(Run_size,
                     PFMC
                     tiers=c(5000,16000,29000,32000,36250,50001),
                     rates=c(100,200,.05,.6,.7,.5,.5)

){


  ifelse(Run_size<=tiers[1],rates[1],
         ifelse(return<=tiers[2],rates[2],
                ifelse(return<=tiers[3],rates[3]*Run_size,
                       ifelse(return<=tiers[4],rates[4]*Run_size,
                              ifelse(return<=tiers[5],rates[5]*Run_size,
                                     ifelse(Run_size<=tiers[6],((rates[6]*(Run_size-29000))),#total harvestable = run size -29000
                                            (rates[7]*((0.75 * (Run_size-50000)) + 21000))
                                     ))))))-PFMC

}
