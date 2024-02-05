#write a function to accept two vectors and find the fourier transform of one w.r.t the other
ft <- function(X, Y, w = F, set.nyquist = T){
  if(length(X)!=length(Y)){
    stop('X and Y have different lengths')
  }else if(is.numeric(X)&is.numeric(Y) == F){
    stop('either X or Y has non-numeric arguments')
  }else if(sum(is.na(X)) != 0){
    stop('remove NAs from X')
  }else if(sum(is.na(Y)) != 0){
    stop('remove NAs from Y')
  }else{ if(sum(diff(diff(X))) != 0){
    warning('non uniform sampling in X: Mean of sampling error vector will be used to calculate sampling rate')
    }
    #calculate sampling freq and establish freq vector
    rn <- diff(X) #calculate the sampling error vector
    N <- length(X) #length of fourier sequence
    Ts <- mean(rn) #sampling time
    if(w == T){
      #fs <- 2*pi/mean(rn)
      w0 <- 2*pi/(N*Ts)
      wf <- seq(0, by=w0, length=N)
      fy <- fft(Y)
      df <- data.frame(wf,fy)
    }else{
      #fs <- 1/mean(rn)
      f0 <- 1/(N*Ts)
      fr <- seq(0, by = f0, length=N)
      fy <- fft(Y)
      df <- data.frame(fr,fy)
    }
    if(set.nyquist==F){
      return(df)
    }else{
      #df_front <- subset(df, df[,1] <= max(df[,1])/2)  ##implement the nyquist criterion when the set.nyquist is left at default
      df_front <- df[1:ceiling(N/2),]
      df_back <- df[(ceiling(N/2) + 1): N,]
      if(w == T){
        df_back$wf <- df_back$wf - N*w0
      }else{
        df_back$fr <- df_back$fr - N*f0
      }
      df <- rbind(df_back, df_front)
      return(df)
    }
  }
}

