

Ricker_fun<-function(S,
                     err,
                     alpha,
                     beta
                     )
                     {

  unlist(exp(alpha-beta*S+err) * S)


}
