#write a program to calculate the inverse fourier transform
ft_inv <- function(wf, fy, w = T, t_start = 0){
  if(length(wf)!=length(fy)){
    stop('freq and fourier coeff columns have different lengths')
  }else if(is.numeric(wf)&is.complex(fy) == F){
    stop('either frequency column is non-numeric or fourier coefficients are non-complex ')
  }else if(sum(is.na(wf)) != 0){
    stop('remove NAs from frequency column')
  }else if(sum(is.na(fy)) != 0){
    stop('remove NAs from fourier coefficients')
  }else if(sum(wf == 0) != 1){
    stop('There must be at least one and only one zero frequency point in the frequency column')
  }else{ if(sum(diff(diff(wf))) != 0){
    warning('non uniform sampling in X: Mean of sampling error vector will be used to calculate sampling rate')
  }
    #calculate sampling time and establish time vector
    rn <- diff(wf) #calculat the samplibng error vector
    N <- length(wf) #length of fourier sequence
    ws <- mean(rn) #minimum frequency step
    if(w == T){
      Ts <- 2*pi/(N*ws) #sampling time
      t <- seq(0, by = Ts, length=N) #time vector
      t <- t + t_start #shift the time vector to the starting point of the function
      df <- data.frame(wf, fy) #build a dataframe
      names(df) <- c('wf', 'fy') #name the data frame appropriately
      df_neg <- subset(df, df$wf < 0) #separate dataframe into +ve and negative frequencies
      df_pos <- subset(df, df$wf >= 0) #separate dataframe into +ve and -ve frequencies
      df_neg$wf <- df_neg$wf + N*ws #convert negative frequencies to positive frequencies
      df <- rbind(df_pos, df_neg) #combine to build new data frame
      Y <- fft(df$fy, inverse = T)/N
      df <- data.frame(t, Y)
      return(df)
    }else{
      Ts <- 1/(N*ws) #sampling time
      t <- seq(0, by = Ts, length=N) #time vector
      t <- t + t_start #shift the time vector to the starting point of the function
      df <- data.frame(wf, fy) #build a dataframe
      names(df) <- c('fr', 'fy') #name the data frame appropriately
      df_neg <- subset(df, df$fr < 0) #separate dataframe into +ve and negative frequencies
      df_pos <- subset(df, df$fr >= 0) #separate dataframe into +ve and -ve frequencies
      df_neg$fr <- df_neg$fr + N*ws #convert negative frequencies to positive frequencies
      df <- rbind(df_pos, df_neg) #combine to build new data frame
      Y <- fft(df$fy, inverse = T)/N
      df <- data.frame(t, Y)
      return(df)
    }
  }
}

