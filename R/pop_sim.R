

#' Simulate summer Chinook population
#'
#' @param index whether to use total or wild abundance index for harvest. options are "total" (default) or "wild"
#' @param n_years integer number of return years to simulate forward
#' @param start_year first brood year of simulation
#' @param init_S 4 x 6 matrix with the spawner (or smolt) abundance of each population in the first six years
#' @param init_PNI PNI values from 14 years leading up to the first year of simulation
#' @param MREER_matrix age x brood year matrix of simulated mature run exploitation rates
#' @param age_prop_array age x population x brood year array of age proportions by population and brood year
#' @param SR_err population x brood year matrix of recruitment deviations
#' @param HOS_model options are "zero" = no hatchery spawning or "HOE" where it hatchery spawners is a funciton of hatchery escapement
#' @param HOS_err population x return year matrix of pHOS errors
#' @param NOB_err population x return year matrix of natural origin broodstock collection errors. Exponentiated!
#' @param pfmc_err vector of return year annual deviations from PFMC AEQ ocean mort abundance
#' @param in_river_harvest_model_option which model of realized vs allowed harvest to use. option 1 is a proportion of allowed fit to data, option 2 is a broken hockey stick fit to data. Option 3 is the same as option 1 but assumes that the expected value is the allowed whereas option 1 assumes that the expected value is less than the allowed.
#' @param implementation_error_scalar a scalar for implementation error relative to the default which was fit to data. default of 1. increase to increase error or decrease to decrease error.
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
#' @param NT_tiers same as above but for non-treaty
#' @param NT_rates  same as above but for non-treaty
#' @param NT_scalar same as above but for non-treaty
#' @param NT_offset same as above but for non-treaty
#' @param NT_share same as above but for non-treaty
#' @param PFMC_include_above the run size above which PFMC AEQ mortality is included in the Non-treaty share.
#'
#' @return returns a list object with results of the simulation and the also includes the hatchery control rule used.
#' @export
#'
#' @examples
#'
#'# the pacific salmon treaty harvest control rule is
#'# 85% of the 2009-2015 average harvest when escapment
#'# goal is not met
#'
#' ## 85% of avg 09-15
#' ave_harv<-internal_data$pfmc_morts_2 |> filter(between(Year,2009,2015)) |>
#'  select(Year, RMRS,`PFMC NT Harvest Total`,In_river_harvest,Sector) |>
#'  pivot_wider(values_from = In_river_harvest,names_from=Sector) |>
#'  mutate(ER=(Treaty+NT)/RMRS) |> summarize(ER_limit=mean(ER)*.85) |>
#'  pull(ER_limit) |> c()
#'
#' ## define harvest control rule
#' PST_HCR<-list( treaty_tiers=c(round(12143/(1-floor((ave_harv)*100)/100)),rep(NA,2)),
#'               treaty_rates=c(floor((ave_harv/2)*100)/100,rep(NA,2)),
#'               treaty_scalar=c(NA,1,rep(NA,1)),
#'               treaty_offset=c(NA,12143,rep(NA,1)),
#'               treaty_share = c(NA,.5,rep(NA,1)),
#'               NT_tiers=c(round(12143/(1-floor((ave_harv)*100)/100)),rep(NA,2)),
#'               NT_rates=c(floor((ave_harv/2)*100)/100,rep(NA,2)),
#'               NT_scalar=c(NA,1,rep(NA,1)),
#'               NT_offset=c(NA,12143,rep(NA,1)),
#'               NT_share = c(NA,.5,rep(NA,1)))
#'
#' ## do simulation
#' PST_sim<-do.call(pop_sim,PST_HCR)
pop_sim<-function(index="total",
                  n_years=25,
                  start_year=2018,
                  n_iter=500,
                  init_S=internal_data$init_S,
                  init_PNI=internal_data$init_PNI,
                  MREER_matrix=internal_data$MREER_out[,,],
                  age_prop_array=internal_data$age_props[,,,],
                  alpha=internal_data$alphas,
                  Rmax=c(1000000,internal_data$Rmax),
                  SR_err=internal_data$SR_err[,,],
                  HOS_model="HOE",
                  HOS_err=internal_data$HOS_err[,,],
                  NOB_err=internal_data$NOB_err[,,],
                  pfmc_err=internal_data$pfmc_err[,],
                  in_river_harvest_model_option=1,
                  implementation_error_scalar =1,
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
                  PFMC_include_above=29000
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
      if(in_river_harvest_model_option==3){
        in_river_harvest_model_coefs<- c(1,1)
      }else{
        if(in_river_harvest_model_option==2){
          in_river_harvest_model_coefs<- internal_data$in_river_coefs
        }
      }
    }

    if(in_river_harvest_model_option%in%c(1,3)){
      in_river_err<-  internal_data$in_river_err_option1[,,]*implementation_error_scalar
    }else{
      in_river_err<- internal_data$in_river_err[,,]*implementation_error_scalar
    }

    # mark selective fisheries matrices
    MS_matrices<-make_marked_um_hrs_fun(release_mort_rate=release_mort_rate,URR=NT_Unmarked_release_rate)
    # total broodstock collection target
    tot_broodstock_target<-HO_broodstock_need+sum(NO_broodstock_target)

    #arrays and matrices to hold results
    NOS<-NOB<-HOS<-pHOS<-pNOB<-array(0,dim=c(3,n_years+6,n_iter),dimnames=list(pop=c("Methow","Okanogan","Wenatchee"),years=seq(from=start_year,by=1,length.out=n_years+6),iter=1:n_iter))

    PFMC<-matrix(NA,n_years+6,n_iter)

    PNI<-array(0,dim=c(3,n_years+14,n_iter),dimnames=list(pop=c("Methow","Okanogan","Wenatchee"),years=seq(from=start_year-8,by=1,length.out=n_years+14),iter=1:n_iter))
    PNI[,1:14,]<-init_PNI

    S<-adult_return<-returns<-HOB<-recruits<-terminal_NT<-terminal_treaty<-escapement<-array(0,dim=c(4,n_years+12,n_iter),dimnames=list(pop=c("Hatchery","Methow","Okanogan","Wenatchee"),years=seq(from=start_year,by=1,length.out=n_years+12),iter=1:n_iter)) # returns will not be complete until year 7 and Spawners and recruits will be 0 in the last 6 years

    # initialise spawners and smolts in first size years
    S[,1:6,] <- t(init_S)
    S[1,-c(1:6,(n_years+(7:12))),] <- smolts[1:(n_years),1:n_iter] # expected smolt releases if broodstock are sufficient. reduced proportionally within population simulations if not enough fish available for broodstocks




    HOB[1,,]<-HO_broodstock_need # HO broodstock collected.reduced  within population simulations if not enough fish available

    for(i in 1:n_iter){
      for (y in 1 : (n_years+6)){


        if(y>6){ # years to simulate managment

          RMRS<- sum(adult_return[,y,i]) #adult river mouth run size
          wild_RMRS<-sum(adult_return[-1,y,i]) #wild adult river mouth run size
          PFMC[y,i]<-sim_PFMC(RMRS,pfmc_err[y,i]) # PFMC AEQ ocean morts (needed to implement HCR)
          # wild_PFMC<-PFMC[y,i]*(wild_RMRS/RMRS)

          if(index=="total"){
            AI<-RMRS+PFMC[y,i]
          }else{
            if(index=="wild"){
            AI<-wild_RMRS#+wild_PFMC[y,i]
            }
          }


          # allowed exploitation based on HCR
          NT_allowed_ER<-allowed_ER(AI,
                                    NT_tiers,
                                    NT_rates,
                                    NT_scalar,
                                    NT_offset,
                                    NT_share)


          Treaty_allowed_ER<-allowed_ER(AI,
                                        treaty_tiers,
                                        treaty_rates,
                                        treaty_scalar,
                                        treaty_offset,
                                        treaty_share)


          Mark_rate= (hatchery_mark_rate*adult_return[1,y,i])/RMRS

          # account for implementation error and come up with harvest rate
          in_river_h_rate<-sim_in_river(model_option=in_river_harvest_model_option,
                                        coefs=in_river_harvest_model_coefs,
                                        allowed_Treaty_ER=Treaty_allowed_ER,
                                        allowed_NT_ER=NT_allowed_ER,
                                        pfmc_AEQ= PFMC[y,i],
                                        RMRS=RMRS,
                                        inlcude_PFMC_above=ifelse(index=="total",PFMC_include_above,0),
                                        in_river_err=in_river_err[,y,i],
                                        mark_rate=Mark_rate,
                                        MS_fisheries_matrices=MS_matrices,
                                        URR=NT_Unmarked_release_rate

          )




          #calculate harvested fish in different sectors
          terminal_NT[1,y,i] <- adult_return[1,y,i] * (((1-hatchery_mark_rate)*in_river_h_rate["NT_unmarked"])+(hatchery_mark_rate*in_river_h_rate["NT_marked"])) #weighted (by hatchery mark rate) mean of unmarked and marked mortality rates
          terminal_NT[2:4,y,i] <- adult_return[2:4,y,i] * in_river_h_rate["NT_unmarked"]
          terminal_treaty[,y,i] <- adult_return[,y,i] * in_river_h_rate["Treaty"]

          escapement[,y,i]<-returns[,y,i]-(terminal_NT[,y,i]+terminal_treaty[,y,i]) #including jacks in escapement but assuming only adults harvested
          NOB[,y,i]<-NOB_fun(escapement[-1,y,i],NOB_err=NOB_err[,y,i],
                             met_target=NO_broodstock_target["Methow"],
                             oka_target=NO_broodstock_target["Okanogan"],
                             wen_target=NO_broodstock_target["Wenatchee"])
          HOB[2:4,y,i]<-pmax(NO_broodstock_target-NOB[,y,i],0)
          NOS[,y,i]<-escapement[-1,y,i]-NOB[,y,i]
          # hatchey broodstock needs for segregated and integrated programs
          tot_HO_broodstock_need<-tot_broodstock_target-sum(NOB[,y,i])
          # Hatchery origin spawners
          HOS[,y,i]<-HOS_fun(mode=HOS_model,HOE = escapement[1,y,i],HOS_err=HOS_err[,y,i])
          #total number of hatchery origin fish needed for broodstock and predicted HOS
          tot_hatch_need<-sum(HOS[,y,i])+tot_HO_broodstock_need

          # is there sufficient hatchery escapement to meet broodstock needs
          if(escapement[1,y,i]<tot_hatch_need){
            # if hatchery escapement does not meet broodstock needs pluys preducted hatchery origin spawners
            # every group is reduced proporitonally
            prop_tot<-escapement[1,y,i]/tot_hatch_need
            HOS[,y,i]<-HOS[,y,i]*prop_tot
            HOB[,y,i]<-HOB[,y,i]*prop_tot
            prop_tot2<-(sum(HOB[,y,i])+sum(NOB[,y,i]))/tot_broodstock_target
            S[1,y,i]<-S[1,y,i]*prop_tot2

          }
          S[2:4,y,i]<-NOS[,y,i]+HOS[,y,i]# *rowMeans(PNI[,(y-6):(y+3),i])
          pHOS[,y,i]<-HOS[,y,i]/(NOS[,y,i]+HOS[,y,i])
          pNOB[,y,i]<-NOB[,y,i]/(NOB[,y,i]+HOB[-1,y,i])
          PNI[,(y+8),i]<- pNOB[,y,i]/( pNOB[,y,i]+ pHOS[,y,i])
        }


        recruits[,y,i]<-Ricker_fun(S[,y,i],SR_err[,y,i],
                                   alpha = alpha,
                                   Rmax = Rmax)

        #apportion to ages
        age_recruits<- recruits[,y,i]*t(age_prop_array[,,y,i]) # populations (rows) by ages (columns)


        # returns (recruits less ocean mortality)
        returns_age_y<-(t(age_recruits) * MREER_matrix[-1,y,i]) # ages (row) by populations (column).

        # building up returns
        ## remember that returns wont be complete until 7th year (start_year+6)
        # returns[,y+3,i]<-returns[,y+3,i]+returns_age_y[1,]
        for ( age in 4:6){ #adults only
          returns[,y+age,i]<-returns[,y+age,i]+returns_age_y[age-2,]
          adult_return[,y+age,i]<-adult_return[,y+age,i]+returns_age_y[age-2,]

        }



      }
    }
    list(
      NOS = NOS,
      HOS=HOS,
      S = S,
      NOB = NOB,
      HOB = HOB,
      pNOB=pNOB,
      pHOS=pHOS,
      PNI =PNI,
      escapement = escapement,
      returns = returns,
      adult_return = adult_return,
      recruits = recruits,
      terminal_NT = terminal_NT,
      terminal_treaty = terminal_treaty,
      PFMC = PFMC,
      HCR = list(treaty_tiers =treaty_tiers,
                 treaty_rates =treaty_rates,
                 treaty_scalar =treaty_scalar,
                 treaty_offset =treaty_offset,
                 treaty_share =treaty_share,
                 NT_tiers =NT_tiers,
                 NT_rates =NT_rates,
                 NT_scalar =NT_scalar,
                 NT_offset =NT_offset,
                 NT_share =NT_share,
                 pfmc_cutoff=PFMC_include_above)
    )
  }, error=function(e){
    return(e)
  })


}
