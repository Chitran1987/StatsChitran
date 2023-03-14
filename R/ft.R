#write a function to accept two vectors and find the fourier transform of one w.r.t the other
ft <- function(X, Y, w = F){
  if(length(X)!=length(Y)){
    stop('X and Y have different lengths')
  }else if(is.numeric(X)&is.numeric(Y) == F){
    stop('either X or Y has non-numeric arguments')
  }else if(sum(is.na(X)) != 0){
    stop('remove NAs from X')
  }else if(sum(is.na(Y)) != 0){
    stop('remove NAs from Y')
  }else{ if(sum(diff(diff(X))) != 0){
    warning('non uniform sampling in X: Mean of sampling error will be used to calculate sampling rate')
    }
    #calculate sampling freq and establish freq vector
    rn <- diff(X) #calculate the sampling difference vector
    if(w == T){
      fs <- 2*pi/mean(rn)
      fr <- seq(0, fs, by=2*pi/((max(X))-(min(X))))
    }else{
      fs <- 1/mean(rn)
      fr <- seq(0, fs, by=1/((max(X))-(min(X))))
    }
    fy <- fft(Y)
    df <- data.frame(fr,fy)
    return(df)
  }
}

