#create a function for implementing a exponential decay probability distribution
exp.prob <- function(X, amp, lambda, probability=F){
  if(probability==T && missing(amp)){
    #This is the probability distribution
    Y <- (lambda/2)*exp(-lambda*abs(X))
  }else if(probability==T && !(missing(amp))){
    stop("The value of amp parameter doesn't matter in an exponential decay distribution")
  }else if(probability==F && missing(amp)){
    stop("No value of amp(scaling factor) parameter")
  }else{
    ##This is simply a statistical distribution
    Y <- amp*exp(-lambda*abs(X))
  }
  return(Y)
}


