

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
                  init_S,
                  MREER_matrix,
                  age_prop_array,
                  SR_err,
                  pHOS_err,
                  NOB_err, #already exponentiation
                  pfmc_err,
                  in_river_err,
                  smolts,
                  ...
                  ){

  n_years<-n_years

  NOS<-NOB<-matrix(0,3,n_years+6,dimnames=list(pop=c("Methow","Okanogan","Wenatchee"),years=seq(from=start_year,by=1,length.out=n_years+6)))

  S<-returns<-recruits<-matrix(0,4,n_years+12,dimnames=list(pop=c("Hatchery","Methow","Okanogan","Wenatchee"),years=seq(from=start_year,by=1,length.out=n_years+12))) # returns will not be complete until year 7 and Spawners and recruits will be 0 in the last 6 years

  S[,1:6] <- (init_S)
  S[1,-c(1:6)] <- smolts

  for (y in 1 : (n_years+6)){


    if(y>=6){

      RMRS<- sum(returns[y,])
      PFMC<-sim_PFMC(RMRS,pfmc_err[y])
      NT_allowed<-allowed_NT(RMRS,PFMC)
      Treaty_allowed<-allowed_Treaty(RMRS)
      in_river_h_rate<-sim_in_river(allowed_Treaty=Treaty_allowed,
                                     allowed_NT_tot=NT_allowed,
                                     PFMC=PFMC,
                                     in_river_err=in_river_err[y,])

      terminal_NT[y,] <- returns[y,] * in_river_h_rate["NT"]
      terminal_treaty[y,] <- returns[y,] * in_river_h_rate["Treaty"]

      escapement<-returns[y,]-terminal_NT[y,]-terminal_treaty[y,]
      NOB[y,]<-NOB_fun(escapement,NOB_err=NOB_err[y,],...)
      NOS[y,]<-escapement[y,]-NOB[y,]
      pHOS<-pHOS_fun(NOS = NOS[y,], HOR = returns[y,1],pHOS_err[y,])
      S[2:4,y]<-(NOS[y,]/(1-pHOS))

    }


    recruits[,y]<-Ricker_fun(S[y,],SR_err[,y],...)

    #apportion to ages
    age_recruits<- recruits[,y]*age_prop_array[,,y]

    #ocean harvest
    ocean_harvest<- t(t(age_recruits) *unlist(MREER_matrix[y,]))

    # returns
    returns_age_y<-age_recruits-ocean_harvest

    # remember that returns wont be complete until 2023
    for ( age in 3:6){
      returns[,y+age]<-returns[,yage]+returns_age_y[,age-2]
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

