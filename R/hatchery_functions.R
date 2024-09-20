

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
#' Title
#'
#' @param model pHOS model. options are
#'  - "zero" no hatchery spawning
#'  - "HOE" hatchery-origin spawners are a function of hatchery escapement, fit to data from  2010-2022
#'  -
#' @param NOS vector of natural origin spawners
#' @param HOE hatchery origin escapement. only need if using model option ""
#' @param pHOS_mod_coefs # coefficients of the pHOS model: intercepts, effect of hatchery escapement only need if using model option ""
#' @param pHOS_err # annual deviations

#'
#' @return
#' @export
#'
#' @examples
#'
pHOS_fun <- function (model,
                      NOS,
                      HOE,
                      pHOS_mod_coefs=internal_data$pHOS_mod_coefs ,
                      pHOS_err
){

  if(model=="zero"){
    rep(0,3)
  }else{
      hoe2<-ifelse(HOE<=0,1,HOE)
 pmax(0,pHOS_mod_coefs[1:3]+pHOS_mod_coefs[4]*log(hoe2)+pHOS_err)
  }


}


#-------------------------------------------------

hatchery_smolt_fun <- function(smolts_mu= internal_data$smolts_mu ,
                               smolts_err){
  exp(smolts_mu+smolts_err)
}
