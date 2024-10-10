

Ricker_fun<-function(S,
                     err,
                     alpha,
                     # beta=c(0,internal_data$beta)
                     Rmax
                     )
                     {

  alpha_vec<-alpha*S
  recruits<-alpha_vec*exp((-alpha_vec/(exp(1)*Rmax))+err)
  recruits[S<50]<-recruits[S<50]*.25

  recruits

}
