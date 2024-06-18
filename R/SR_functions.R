

Ricker_fun<-function(S,
                     err,
                     alpha=c(21.588566,2.557226,2.557226,2.557226),
                     beta=c(0, 3.654050e-04, 9.879599e-05, 7.346235e-05)
                     )
                     {

  S*alpha*exp(-beta*S+err)

}
