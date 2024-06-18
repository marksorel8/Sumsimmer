

# natural origin broodstock
NOB_fun<-function(escapement, met_target = 122, oka_target=650, wen_target = 310,NOB_err){

  # for methow and wenatchee, use 2024 NOB goals but not to exceed 30% of escapement. For okanogan using a declining function from the escapement goal based on the data.
  methow <- min(c((.3*escapement[1]),met_target))*exp(NOB_err[1]*.05)  #apply 5% error

  okanogan <- min(c((.3*escapement[2]),oka_target,exp(.713*log(escapement[2]))))*exp(NOB_err[2]*.05)

  wenatchee <- min(c((.3*escapement[3]),wen_target))*exp(NOB_err[3]*.05)

  return(c(methow,okanogan,wenatchee))
}



#---------------------------------------------------


#proportion of hatchery origin spawners. Assumes available.
pHOS_fun <- function (NOS, #natural origin spawners
                      HOR, # hatchery origin returns
                      ints=pHOS_mod$fit$par[1:3], #intercepts
                      slopes=pHOS_mod$fit$par[5:7], # effect of NOS
                      hatch_effect=pHOS_mod$fit$par[4], # effect of HOR
                      pop_sd=exp(pHOS_mod$fit$par[8:10]), # process error sd
                      pHOS_err
)
{

  plogis(ints+slopes*log(NOS)+hatch_effect*log(HOR)+pHOS_err*pop_sd)

}


#-------------------------------------------------

hatchery_smolt_fun <- function(smolts_mu,
                               smolts_sd,
                               smolts_err){
 exp(smolts_mu+smolts_err*smolts_sd)
}
