#create a function for implementing a exponential decay probability distribution
exp.prob <- function(X, amp, lambda, x_0 = 0.0, probability=F){
  if(probability==T && missing(amp)){
    #This is the probability distribution
    Y <- (lambda/2)*exp(-lambda*abs(X - x_0))
  }else if(probability==T && !(missing(amp))){
    stop("The value of amp parameter doesn't matter in a probability distribution")
  }else if(probability==F && missing(amp)){
    stop("No value of amp(scaling factor) parameter")
  }else{
    ##This is simply a statistical distribution
    Y <- amp*exp(-lambda*abs(X - x_0))
  }
  return(Y)
}


