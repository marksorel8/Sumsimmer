

# natural origin broodstock
NOB_fun<-function(escapement,
                  met_target = 122,
                  oka_target=650,
                  wen_target = 310,
                  NOB_err){

  # for methow and wenatchee, use 2024 NOB goals but not to exceed 30% of escapement. For okanogan using a declining function from the escapement goal based on the data.
  methow <- min(c((.3*escapement[1]),met_target))*NOB_err[1]

  okanogan <- min(c((.3*escapement[2]),oka_target,exp(.713*log(escapement[2]))))*NOB_err[2])

  wenatchee <- min(c((.3*escapement[3]),wen_target))*NOB_err[3]

  c(methow,okanogan,wenatchee)
}



#---------------------------------------------------


#proportion of hatchery origin spawners. Assumes available.
pHOS_fun <- function (NOS, #natural origin spawners
                      HOR, # hatchery origin returns
                      ints=c(4.9158452, 5.2047245, 0.8179949), #intercepts
                      slopes= c(-1.2659862, -1.1420163, -0.7434057), # effect of NOS
                      hatch_effect=0.3469403 , # effect of HOR
                      pHOS_err
)
{

  plogis(ints+slopes*log(NOS)+hatch_effect*log(HOR)+pHOS_err)

}


#-------------------------------------------------

hatchery_smolt_fun <- function(smolts_mu= 7.962251 ,
                               smolts_err){
 exp(smolts_mu+smolts_err)
}
