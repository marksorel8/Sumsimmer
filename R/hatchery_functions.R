

# natural origin broodstock
NOB_fun<-function(escapement,
                  met_target = 122,
                  oka_target=650,
                  wen_target = 310,
                  Ok_BS_coef=internal_data$Ok_BS_coef,
                  NOB_err){

  # for methow and wenatchee, use 2024 NOB goals but not to exceed 30% of escapement. For okanogan using a declining function from the escapement goal based on the data.
  methow <- min(c((.3*escapement[1]),met_target))*NOB_err[1]

  okanogan <- min(c((.3*escapement[2]),oka_target,exp(Ok_BS_coef*log(escapement[2]))))*NOB_err[2]

  wenatchee <- min(c((.3*escapement[3]),wen_target))*NOB_err[3]

  c(methow,okanogan,wenatchee)
}



#---------------------------------------------------


#proportion of hatchery origin spawners. Assumes available.
pHOS_fun <- function (NOS, #natural origin spawners
                      HOR, # hatchery origin returns
                      pHOS_mod_coefs=internal_data$pHOS_mod_coefs , # intercepts, effect of hatchery return
                      pHOS_err
){

  plogis(pHOS_mod_coefs[1:3]+pHOS_mod_coefs[4]*log(HOR)+pHOS_mod_coefs[5:7]*log(NOS)+pHOS_err)

}


#-------------------------------------------------

hatchery_smolt_fun <- function(smolts_mu= internal_data$smolts_mu ,
                               smolts_err){
 exp(smolts_mu+smolts_err)
}
