

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
#' @param in_river_err 2 x year matrix of return year errors in in-river harvest for treaty and non-treaty
#' @param smolts vector of brood year annual smolt releases
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
pop_sim<-function(n_years=25,
                  start_year=2017,
                  init_S=internal_data$init_S,
                  MREER_matrix=internal_data$MREER_out[,,1],
                  age_prop_array=internal_data$age_props[,,,1],
                  SR_err=internal_data$SR_err[,,1],
                  pHOS_err=internal_data$pHOS_err[,,1],
                  NOB_err=internal_data$NOB_err[,,1], #already exponentiation
                  pfmc_err=internal_data$pfmc_err[,1],
                  in_river_err=internal_data$in_river_err[,,1],
                  smolts=hatchery_smolt_fun(smolts_err =internal_data$smolt_err)[,1],
                  hatchery_mark_rate = internal_data$hatch_MR_mu,
                  HO_broodstock_need = 2000,
                  NO_broodstock_target = c("Methow"=122,"Okanogan" = 650, "Wenatchee" = 310),
                  ...
                  ){

  NOS<-NOB<-matrix(0,3,n_years+6,dimnames=list(pop=c("Methow","Okanogan","Wenatchee"),years=seq(from=start_year,by=1,length.out=n_years+6)))

  PFMC<-numeric(n_years+6)

  S<-returns<-recruits<-terminal_NT<-terminal_treaty<-matrix(0,4,n_years+12,dimnames=list(pop=c("Hatchery","Methow","Okanogan","Wenatchee"),years=seq(from=start_year,by=1,length.out=n_years+12))) # returns will not be complete until year 7 and Spawners and recruits will be 0 in the last 6 years

  S[,1:6] <- t(init_S)
  S[1,-c(1:6,(n_years+(7:12)))] <- smolts[1:(n_years)]

  for (y in 1 : 6){


    if(y>6){

      RMRS<- sum(returns[,y])
      PFMC[y]<-sim_PFMC(RMRS,pfmc_err[y])
      NT_allowed<-allowed_NT(RMRS,PFMC[y])
      Treaty_allowed<-allowed_Treaty(RMRS)
      Mark_rate= (hatchery_mark_rate*returns[1,y])/RMRS
      in_river_h_rate<-sim_in_river(allowed_Treaty=Treaty_allowed,
                                    NT_allowed_in_river=NT_allowed,
                                     in_river_err=in_river_err[,y],
                                    mark_rate=Mark_rate,
                                    RMRS=RMRS)

      terminal_NT[1,y] <- returns[1,y] * (((1-hatchery_mark_rate)*in_river_h_rate["NT_unmarked"])+(hatchery_mark_rate*in_river_h_rate["NT_marked"])) #weighted (by hatchery mark rate) mean of unmarked and marked mortality rates
      terminal_NT[2:4,y] <- returns[2:4,y] * in_river_h_rate["NT_unmarked"]
      terminal_treaty[,y] <- returns[,y] * in_river_h_rate["Treaty"]

      escapement<-returns[,y]-terminal_NT[,y]-terminal_treaty[,y]
      NOB[,y]<-NOB_fun(escapement[-1],NOB_err=NOB_err[,y],
                       NO_broodstock_target["Methow"],
                       NO_broodstock_target["Okanogan"],
                       NO_broodstock_target["Wenatchee"])
      NOS[,y]<-escapement[-1]-NOB[,y]
      # is there sufficient hatchery escapement to meet broodstock needs
       if(escapement[1]<HO_broodstock_need){
        S[1,y]<-S[1,y]*(escapement[1]/HO_broodstock_need)
        HOS<-c(0,0,0)
       }else{ # if hatchery escapement exceed broodstock needs,hatchery origin spawners

            }

      pHOS<-pHOS_fun(NOS = NOS[,y], HOR = returns[1,y],pHOS_err=pHOS_err[,y]) # predicted pHOS
      HOS<-NOS[,y]*((1/(1-pHOS))-1) # predicted HOS


      if(sum(HOS)<escapement[1]-HO_broodstock_need) # check if hatchery escapement sufficient
      S[2:4,y]<-(NOS[,y]/(1-pHOS))


    }


    recruits[,y]<-Ricker_fun(S[,y],SR_err[,y])

    #apportion to ages
    age_recruits<- recruits[,y]*t(age_prop_array[,,y]) # populations (rows) by ages (columns)


    # returns
    returns_age_y<-(t(age_recruits) * MREER_matrix[-1,y]) # ages (row) by populations (column).

    # remember that returns wont be complete until 7th year (start_year+6)
    for ( age in 4:6){ #adults only
      returns[,y+age]<-returns[,y+age]+returns_age_y[age-2,]
    }


  }


  list(
    NOS = NOS,
    S = S,
    NOB = NOB,
    returns = returns,
    recruits = recruits,
    terminal_non_treaty = terminal_non_treaty,
    terminal_treaty = terminal_treaty
  )


}

