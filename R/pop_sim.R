

#' Simulate summer Chinook population
#'
#' @param n_years integer number of return years to simulate forward
#' @param start_year first brood year of simulation
#' @param init_S 4 x 6 matrix with the spawner (or smolt) abundance of each population in the first six years
#' @param MREER_matrix age x brood year matrix of simulated mature run exploitation rates
#' @param age_prop_array age x population x brood year array of age proportions by population and brood year
#' @param SR_err population x brood year matrix of recruitment deviations
#' @param pHOS_err population x return year matrix of pHOS errors
#' @param NOB_err population x return year matrix of natural origin broodstock collection errors. Exponentiated!
#' @param pfmc_err vector of return year annual deviations from PFMC AEQ ocean mort abundance
#' @param in_river_harvest_model_option which model of realized vs allowed harvest to use
#' @param smolts vector of brood year annual smolt releases
#' @param n_iter  number of population projections do to. Max possible is 500, the default
#' @param hatchery_mark_rate    real between 0 and 1. proprotion of hatchery origin fish that are adipose clipped
#' @param HO_broodstock_need    integer, number of broodstock needed to produce smolt release targets
#' @param NO_broodstock_target  named vector of three integers. number of natural origin broodstock needed to produce smolt releases for conservation programs. Values in vector must have names "Methow","Okanogan", and "Wenatchee"
#' @param release_mort_rate real between 0 and 1. post release mortality in sport fishery
#' @param NT_Unmarked_release_rate  real between 0 and 1. proporiton of unmarked fish handled in non-treaty fishery that are released
#' @param treaty_tiers the run sizes below which the rates in the tier are applied
#' @param treaty_rates the harvest rate for the tier, if applicable. otherwise NA
#' @param treaty_scalar if the allowable catch is a function of the run size, as in the two highest tiers of the current rule, this number is multiplied by the run size
#' @param treaty_offset if the allowable catch is a function of the run size, as in the two highest tiers of the current rule, this number is subtracted from the scaled run size
#' @param treaty_share if the allowable catch is a function of the run size, as in the two highest tiers of the current rule, this number is multipled by the scales run size less the offset
#' @param NT_tiers
#' @param NT_rates
#' @param NT_scalar
#' @param NT_offset
#' @param NT_share
#' @param ...
#'
#' @return
#' @export
#'
#' @examples

pop_sim<-function(n_years=25,
                  start_year=2017,
                  n_iter=500,
                  init_S=internal_data$init_S,
                  MREER_matrix=internal_data$MREER_out[,,],
                  age_prop_array=internal_data$age_props[,,,],
                  SR_err=internal_data$SR_err[,,],
                  pHOS_err=internal_data$pHOS_err[,,],
                  NOB_err=internal_data$NOB_err[,,], #already exponentiation
                  pfmc_err=internal_data$pfmc_err[,],
                  in_river_harvest_model_option=1,
                  smolts=hatchery_smolt_fun(smolts_err =internal_data$smolt_err),
                  hatchery_mark_rate = internal_data$hatch_MR_mu,
                  HO_broodstock_need = 2000,
                  NO_broodstock_target = c("Methow"=122,"Okanogan" = 650, "Wenatchee" = 310),
                  release_mort_rate = 0.15,
                  NT_Unmarked_release_rate=internal_data$URR,
                  treaty_tiers=c(16000,36250,50000,Inf,rep(NA,3)),
                  treaty_rates=c(.05,.1,NA,NA,rep(NA,3)),
                  treaty_scalar=c(NA,NA,1,.75,rep(NA,3)),
                  treaty_offset=c(NA,NA,29000,16500,rep(NA,3)),
                  treaty_share = c(NA,NA,.5,.5,rep(NA,3)),
                  NT_tiers=c(5000,16000,29000,32000,36250,50001,Inf),
                  NT_rates=c(100,200,.05,.06,.07,NA,NA),
                  NT_scalar=c(rep(NA,5),1,.75),
                  NT_offset=c(rep(NA,5),29000,16500),
                  NT_share=c(rep(NA,5),.5,.5),
                  ...
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


    #stuff to do with option for inriver harvest error model
    if(in_river_harvest_model_option==1){
      in_river_harvest_model_coefs<- internal_data$in_river_coefs_option1
    }else{
      in_river_harvest_model_coefs<- internal_data$in_river_coefs
    }

    if(in_river_harvest_model_option==1){
      in_river_err<-  internal_data$in_river_err_option1[,,]
    }else{
      in_river_err<- internal_data$in_river_err[,,]
    }

# mark selective fisheries matrices
    MS_matrices<-make_marked_um_hrs_fun(release_mort_rate=release_mort_rate,URR=NT_Unmarked_release_rate)
# total broodstock collection target
    tot_broodstock_target<-HO_broodstock_need+sum(NO_broodstock_target)


    NOS<-NOB<-array(0,dim=c(3,n_years+6,n_iter),dimnames=list(pop=c("Methow","Okanogan","Wenatchee"),years=seq(from=start_year,by=1,length.out=n_years+6),iter=1:n_iter))

    PFMC<-matrix(NA,n_years+6,n_iter)

    S<-returns<-HOB<-recruits<-terminal_NT<-terminal_treaty<-array(0,dim=c(4,n_years+12,n_iter),dimnames=list(pop=c("Hatchery","Methow","Okanogan","Wenatchee"),years=seq(from=start_year,by=1,length.out=n_years+12),iter=1:n_iter)) # returns will not be complete until year 7 and Spawners and recruits will be 0 in the last 6 years


    S[,1:6,] <- t(init_S)
    S[1,-c(1:6,(n_years+(7:12))),] <- smolts[1:(n_years),1:n_iter]
    HOB[1,,]<-HO_broodstock_need

    for(i in 1:n_iter){
      for (y in 1 : (n_years+6)){


        if(y>6){

          RMRS<- sum(returns[,y,i])
          PFMC[y,i]<-sim_PFMC(RMRS,pfmc_err[y,i])



          NT_allowed_ER<-allowed_ER(RMRS+PFMC[y,i],
                                    NT_tiers,
                                    NT_rates,
                                    NT_scalar,
                                    NT_offset,
                                    NT_share)


          Treaty_allowed_ER<-allowed_ER(RMRS+PFMC[y,i],
                                        treaty_tiers,
                                        treaty_rates,
                                        treaty_scalar,
                                        treaty_offset,
                                        treaty_share)


          Mark_rate= (hatchery_mark_rate*returns[1,y,i])/RMRS
          in_river_h_rate<-sim_in_river(model_option=in_river_harvest_model_option,
                                        coefs=in_river_harvest_model_coefs,
                                        allowed_Treaty_ER=Treaty_allowed_ER,
                                        allowed_NT_ER=NT_allowed_ER,
                                        pfmc_AEQ= PFMC[y,i],
                                        RMRS=RMRS,
                                        in_river_err=in_river_err[,y,i],
                                        mark_rate=Mark_rate,
                                        MS_fisheries_matrices=MS_matrices,
                                        URR=NT_Unmarked_release_rate

                                        )





          terminal_NT[1,y,i] <- returns[1,y,i] * (((1-hatchery_mark_rate)*in_river_h_rate["NT_unmarked"])+(hatchery_mark_rate*in_river_h_rate["NT_marked"])) #weighted (by hatchery mark rate) mean of unmarked and marked mortality rates
          terminal_NT[2:4,y,i] <- returns[2:4,y,i] * in_river_h_rate["NT_unmarked"]
          terminal_treaty[,y,i] <- returns[,y,i] * in_river_h_rate["Treaty"]

          escapement<-returns[,y,i]-(terminal_NT[,y,i]+terminal_treaty[,y,i])
          NOB[,y,i]<-NOB_fun(escapement[-1],NOB_err=NOB_err[,y,i],
                             met_target=NO_broodstock_target["Methow"],
                             oka_target=NO_broodstock_target["Okanogan"],
                             wen_target=NO_broodstock_target["Wenatchee"])
          HOB[2:4,y,i]<-pmax(NO_broodstock_target-NOB[,y,i],0)
          NOS[,y,i]<-escapement[-1]-NOB[,y,i]
          # hatchey broodstock needs for segregated and integrated programs
          tot_HO_broodstock_need<-tot_broodstock_target-sum(NOB[,y,i])
          # predicted pHOS
          pHOS<-pHOS_fun(NOS = NOS[,y,i], HOE = escapement[1],pHOS_err=pHOS_err[,y,i])
          #predicted Hatchery origin spawners
          HOS<-NOS[,y,i]*((1/(1-pHOS))-1)
          #total number of hatchery origin fish needed for broodstock and predicted HOS
          tot_hatch_need<-sum(HOS)+tot_HO_broodstock_need

          # if there is there sufficient hatchery escapement to meet broodstock needs
          if(escapement[1]<tot_hatch_need){
            # if hatchery escapement does not meet broodstock needs pluys preducted hatchery origin spawners
            # every group is reduced proporitonally
            prop_tot<-escapement[1]/tot_hatch_need
            HOS<-HOS*prop_tot
            HOB[,y,i]<-HOB[,y,i]*prop_tot
            prop_tot2<-(sum(HOB[,y,i])+sum(NOB[,y,i]))/tot_broodstock_target
            S[1,y,i]<-max(S[1,y,i]*prop_tot2,S[1,y,i])

          }
          S[2:4,y,i]<-NOS[,y,i]+HOS

        }


        recruits[,y,i]<-Ricker_fun(S[,y,i],SR_err[,y,i])

        #apportion to ages
        age_recruits<- recruits[,y,i]*t(age_prop_array[,,y,i]) # populations (rows) by ages (columns)


        # returns
        returns_age_y<-(t(age_recruits) * MREER_matrix[-1,y,i]) # ages (row) by populations (column).

        # remember that returns wont be complete until 7th year (start_year+6)
        for ( age in 4:6){ #adults only
          returns[,y+age,i]<-returns[,y+age,i]+returns_age_y[age-2,]
        }



      }
    }
    list(
      NOS = NOS,
      S = S,
      NOB = NOB,
      HOB = HOB,
      returns = returns,
      recruits = recruits,
      terminal_NT = terminal_NT,
      terminal_treaty = terminal_treaty,
      PFMC=PFMC
    )
  }, error=function(e){
    return(e)
  })


}
