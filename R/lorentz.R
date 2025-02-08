#create a function for implementing a lorentzian
lorentz <- function(X, amp, x_0, gamm, probability = F){
  ###error checking
  #X is a numeric vector
  if( !is.numeric(X) ){
    stop('X must be a numeric vector')
  }
  #amp is a scalar numeric but only if probability if F
  if( probability == F ){
    if( !is.numeric(amp) || length(amp)!=1 ){
      stop('amp must be a scalar numeric')
    }
  }
  #x_0 is a scalar numeric
  if( !is.numeric(x_0) || length(x_0)!=1 ){
    stop('x_0 must be a scalar numeric')
  }
  #gamm is a scalar numeric
  if( !is.numeric(gamm) || length(gamm)!=1 ){
    stop('gamm must be a scalar numeric')
  }
  #probability is a scalar boolean
  if( !is.logical(probability) || length(probability)!=1 ){
    stop('probability must be a boolean bit')
  }
  ###If probability is T, amp needs to be missing, if not amp value needed
  if(probability==T && missing(amp)){
    amp <- 1
  }else if(probability==T && !(missing(amp))){
    stop("The value of amp parameter doesn't matter in a normalized probability distribution")
  }else if(probability==F && missing(amp)){
    stop("Argument amp is needed, when probability is set to FALSE")
  }else{
    ##everything okay
  }
  ###actual code
  Y <- (amp/(pi*gamm))*(1/(1 + ((X - x_0)/(gamm))^2))
  return(Y)
}
