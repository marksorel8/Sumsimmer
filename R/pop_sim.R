

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
#' @param n_iter
#' @param hatchery_mark_rate
#' @param HO_broodstock_need
#' @param NO_broodstock_target
#' @param ...
#'
#' @return
#' @export
#'
#' @examples

pop_sim<-function(n_years=25,
                  start_year=2017,
                  n_iter=100,
                  init_S=internal_data$init_S,
                  MREER_matrix=internal_data$MREER_out[,,],
                  age_prop_array=internal_data$age_props[,,,],
                  SR_err=internal_data$SR_err[,,],
                  pHOS_err=internal_data$pHOS_err[,,],
                  NOB_err=internal_data$NOB_err[,,], #already exponentiation
                  pfmc_err=internal_data$pfmc_err[,],
                  in_river_err=internal_data$in_river_err[,,],
                  smolts=hatchery_smolt_fun(smolts_err =internal_data$smolt_err),
                  hatchery_mark_rate = internal_data$hatch_MR_mu,
                  HO_broodstock_need = 2000,
                  NO_broodstock_target = c("Methow"=122,"Okanogan" = 650, "Wenatchee" = 310),
                  ...
                  ){

  NOS<-NOB<-array(0,dim=c(3,n_years+6,n_iter),dimnames=list(pop=c("Methow","Okanogan","Wenatchee"),years=seq(from=start_year,by=1,length.out=n_years+6),iter=1:n_iter))

  PFMC<-matrix(NA,n_years+6,n_iter)

  S<-returns<-recruits<-terminal_NT<-terminal_treaty<-array(0,dim=c(4,n_years+12,n_iter),dimnames=list(pop=c("Hatchery","Methow","Okanogan","Wenatchee"),years=seq(from=start_year,by=1,length.out=n_years+12),iter=1:n_iter)) # returns will not be complete until year 7 and Spawners and recruits will be 0 in the last 6 years

  S[,1:6,] <- t(init_S)
  S[1,-c(1:6,(n_years+(7:12))),] <- smolts[1:(n_years),1:n_iter]


for(i in 1:n_iter){
  for (y in 1 : (n_years+6)){


    if(y>6){

      RMRS<- sum(returns[,y,i])
      PFMC[y,i]<-sim_PFMC(RMRS,pfmc_err[y,i])
      NT_allowed<-allowed_NT(RMRS,PFMC[y,i])
      Treaty_allowed<-allowed_Treaty(RMRS)
      Mark_rate= (hatchery_mark_rate*returns[1,y,i])/RMRS
      in_river_h_rate<-sim_in_river(allowed_Treaty=Treaty_allowed,
                                    NT_allowed_in_river=NT_allowed,
                                     in_river_err=in_river_err[,y,i],
                                    mark_rate=Mark_rate)

      terminal_NT[1,y,i] <- returns[1,y,i] * (((1-hatchery_mark_rate)*in_river_h_rate["NT_unmarked"])+(hatchery_mark_rate*in_river_h_rate["NT_marked"])) #weighted (by hatchery mark rate) mean of unmarked and marked mortality rates
      terminal_NT[2:4,y,i] <- returns[2:4,y,i] * in_river_h_rate["NT_unmarked"]
      terminal_treaty[,y,i] <- returns[,y,i] * in_river_h_rate["Treaty"]

      escapement<-returns[,y,i]-terminal_NT[,y,i]-terminal_treaty[,y,i]
      NOB[,y,i]<-NOB_fun(escapement[-1],NOB_err=NOB_err[,y,i],
                       met_target=NO_broodstock_target["Methow"],
                       oka_target=NO_broodstock_target["Okanogan"],
                       wen_target=NO_broodstock_target["Wenatchee"])
      NOS[,y,i]<-escapement[-1]-NOB[,y,i]
      # is there sufficient hatchery escapement to meet broodstock needs
      HO_broodstock_need_2<-HO_broodstock_need+(sum(NO_broodstock_target)-sum(NOB[,y,i]))
       if(escapement[1]<HO_broodstock_need_2){
        S[1,y,i]<-S[1,y,i]*(escapement[1]/HO_broodstock_need_2)
       }else{ # if hatchery escapement exceed broodstock needs,hatchery origin spawners
       pHOS<-pHOS_fun(NOS = NOS[,y,i], HOR = returns[1,y,i],pHOS_err=pHOS_err[,y,i]) # predicted pHOS
       HOS_max<-escapement[1]-HO_broodstock_need_2
       HOS<-NOS[,y,i]*((1/(1-pHOS))-1)
       if(sum(HOS)>HOS_max){
         HOS<-proportions(HOS)*HOS_max
       }
       S[2:4,y,i]<-NOS[,y,i]+HOS
            }

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
    returns = returns,
    recruits = recruits,
    terminal_NT = terminal_NT,
    terminal_treaty = terminal_treaty,
    PFMC=PFMC
  )


}

