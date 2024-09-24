

Ricker_fun<-function(S,
                     err,
                     alpha=internal_data$alpha,
                     # beta=c(0,internal_data$beta)
                     Rmax=c(1000000,internal_data$Rmax)
                     )
                     {

  alpha_vec<-alpha*S
  recruits<-alpha_vec*exp((-alpha_vec/(exp(1)*Rmax))+err)
  recruits[S<50]<-1

  recruits

}
