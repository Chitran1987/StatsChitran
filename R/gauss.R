#create a function for implementing a gaussian
gauss <- function(X, amp, mu=0, sig=1, probability=F){
  if(probability==T && missing(amp)){
    amp <- 1/(sig*sqrt(2*pi))
  }else if(probability==T && !(missing(amp))){
    stop("The value of amp parameter doesn't matter in a normal distribution")
  }else if(probability==F && missing(amp)){
    stop("No value of amp(scaling factor) parameter")
  }else{
    ##everything okay
  }
  y <- amp*exp(-1*((X - mu)/(sqrt(2)*sig))^2)
  return(y)
}


