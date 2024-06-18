
#' Title
#'
#' @param n_years
#' @param start_year
#' @param init_S
#' @param MREER_matrix
#' @param age_prop_array
#' @param SR_err
#' @param pHOS_err
#' @param NOB_err
#' @param pfmc_err
#' @param in_river_err
#' @param smolts_err
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
pop_sim<-function(n_years=25,
                  start_year=2017,
                  init_S,
                  MREER_matrix, # simulatered MREER by age and year
                  age_prop_array,
                  SR_err,
                  pHOS_err,
                  NOB_err, #already exponentiation
                  pfmc_err,
                  in_river_err,
                  smolts_err,
                  ...
                  ){

  n_years<-n_years+6

  NOS<-NOB<-matrix(0,3,n_years,dimnames=list(pop=c("Methow","Okanogan","Wenatchee"),years=seq(from=start_year,by=1,length.out=n_years)))

  S<-returns<-recruits<-matrix(0,4,n_years,dimnames=list(pop=c("Hatchery","Methow","Okanogan","Wenatchee"),years=seq(from=start_year,by=1,length.out=n_years)))

  S[,1:6] <- (init_S)


  for (y in 1 : 6){


    if(y>=6){

      RMRS<- sum(returns[y,])
      PFMC<-sim_PFMC(RMRS,pfmc_err[y])
      NT_allowed<-allowed_NT(RMRS,PFMC)
      Treaty_allowed<-allowed_Treaty(RMRS)
      in_river_harvest<-sim_in_river(allowed_Treaty=Treaty_allowed,
                                     allowed_NT_tot=NT_allowed,
                                     PFMC=PFMC,
                                     in_river_err=in_river_err[y,])

      terminal_non_treaty[y,] <-returns[y,] * in_river_harvest["NT"]
      terminal_treaty[y,] <-returns[y,] * in_river_harvest["Treaty"]

      escapement<-returns[y,]-terminal_non_treaty[y,]-terminal_treaty[y,]
      NOB[y,]<-NOB_fun(escapement,NOB_err=NOB_err[y,],...)
      NOS[y,]<-escapement[y,]-NOB[y,]
      pHOS<-pHOS_fun(NOS = NOS[y,], HOR = returns[y,1],pHOS_err[y,])
      S[y,2:4]<-(NOS[y,]/(1-pHOS))
      S[y,1]<-(smolts_err=smolts_err[y])

    }


    recruits[,y]<-Ricker_fun(S[y,],SR_err[,y],...)

    #apportion to ages
    age_recruits<- recruits[,y]*age_prop_array[,,y+31]

    #ocean harvest
    ocean_harvest<- t(t(age_recruits) *unlist(MREER_matrix[y+48,]))

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

