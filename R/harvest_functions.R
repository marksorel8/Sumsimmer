



sim_PFMC<-function(RMRS,
                   pfmc_err,
                    PFMC_ave_prop=0.7570474
                    ){

  exp(PFMC_ave_prop*log(RMRS)+pfmc_err)
}



sim_in_river<-function(allowed_Treaty,
                       allowed_NT_tot,
                       PFMC,
                       in_river_err,
                       coefs=c(Intercept=6316.1067775,SectorTreaty=-425.7992964,
                       Allowed_in_river=0.2882035,U1.Allowed_in_river=0.6436054),
                       psi=1497
                       ){


  NT_allowed_in_river<-allowed_NT_tot-PFMC

  #Segmented model of expected actual harvest as a funciton of allowed).
  #Non-treaty
  NT<-coefs[1]+NT_allowed_in_river*coefs[3]+ifelse(NT_allowed_in_river>psi,NT_allowed_in_river*coefs[4],0)
  #Treaty
  Treaty<- coefs[1]+coefs[2]+allowed_Treaty*coefs[3]+ifelse(allowed_Treaty>psi,allowed_Treaty*coefs[4],0)

  #going with truncated normal errors because I would foresee management error in terms of numbers of fish being relatively constant across allowed harvest rates.
  c(NT=max(NT+(in_river_err[1]),0),
    Treaty=max(Treaty+(in_river_err[2]),0))
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
                     PFMC,
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
