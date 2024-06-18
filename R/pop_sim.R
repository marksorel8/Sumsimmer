#spawners in first 6 years

init_S<-esc_dat |> filter(between(year,start_year,start_year+5)) |>
  select(year,basin=population_name,total_spawners) |>
  arrange(basin) |> pivot_wider(names_from = basin,values_from=total_spawners) |>
  left_join(rel3 |> select(year=brood_year,Hatchery=releases)) |>
  select(Hatchery,Methow,Okanogan,Wenatchee)
##whoa! 2022 total spawners in methow in 2022. Doesn't appear to be a data entry mistake!


pop_sim<-function(n_years,
                  start_year=2017,
                  init_S,
                  smolts_mu,
                  smolts_sd){

  NOS<-HOS<-NOB<-HOB<-matrix(0,3,n_years,dimnames=list(pop=c("Methow","Okanogan","Wenatchee"),years=seq(from=start_year,by=1,length.out=n_years)))
  smolts_h<-numeric(n_years) |> `names<-`(seq(from=start_year,by=1,length.out=n_years))
  S<-returns<-ocean_harv<-recruits<-matrix(0,4,n_years,dimnames=list(pop=c("Hatchery","Methow","Okanogan","Wenatchee"),years=seq(from=start_year,by=1,length.out=n_years)))

  S[,1:6] <- t(init_S)
  S[1,-c(1:5)]<-exp(rnorm(n_years-5,smolts_mu,smolts_sd)) #missing 2022 hatchery releases so simulating those


  for (y in 1 : 6){


    if(y>=6){

      RMRS<- sum(returns[y,])
      PFMC<-sim_PFMC(RMRS)

      terminal_non_treaty[y,] <-returns[y,] * non_treaty_HR(RMRS,PFMC)
      terminal_treaty[y,] <-returns[y,] * treaty_HR(RMRS)

      escapement[y,]<-returns[y,]-terminal_non_treaty[y,]-terminal_treaty[y,]
      NOB[y,]<-NOB_fun(escapement[y,])
      NOS[y,]<-escapement[y,]-NOB[y,]
      pHOS[y,]<-pHOS_fun(NOS = NOS[y,], HOR = returns[y,1])
      S[y,2:4]<-(NOS[y,]/(1-pHOS[y,]))

    }


    recruits[,y]<-unlist(exp(alpha-beta*init_S[y,]+SR_err[,y,1]) *
                           init_S[y,])

    #apportion to ages
    age_recruits<- recruits[,y]*age_prop_array[,,y+31]

    #ocean harvest
    ocean_harvest<- t(t(age_recruits) *unlist(MREER_matrix[y+48,]))

    # returns
    returns_y<-age_recruits-ocean_harvest

    # remember that returns wont be complete until 2023
    for ( i in 1:4){
      returns[,y+2+i]<-returns[,y+2+i]+returns_y[,i]
    }


  }

}
