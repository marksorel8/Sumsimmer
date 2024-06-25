

Ricker_fun<-function(S,
                     err,
                     alpha=internal_data$alpha,
                     beta=c(0,internal_data$beta)
                     )
                     {

  S*alpha*exp(-beta*S+err)

}
