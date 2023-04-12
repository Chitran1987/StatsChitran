#Create an approximate numeric integration function
num_integrate <- function(X,Y, xmin, xmax, type='avg'){
  ##error handling
  if(xmin < min(X)){
    stop('Lower bound of integration not in domain')
  }
  if(xmax > max(X)){
    stop('Upper bound of integration not in domain')
  }
  if(length(X) != length(Y)){
    stop('X and Y are not of the same lengths')
  }
  if(abs(xmax - xmin) < 4*mean(diff(X))){
    stop('Not enough samples to measure an integral')
  }
  #####################################################################
  ##subseting the data into the upper(xmax) and lower(xmin) limits####
  df <- data.frame(X,Y)
  names(df) <- c('X', 'Y')
  df <- subset(df, (df$X >= xmin) & (df$X <= xmax))
  #####################################################################
  ##extra error checking after subsetting because of the case of non-uniform sampling######
  if(length(df$X) < 4){
    stop('Not enough samples with probably non-uniform sampling')
  }
  s <- 0 #sum to update the area of the cumulative reimann rectangles
  if(type == 'right'){
  for (i in 1:(length(df$X)-1)){
    ar <- df$Y[i]*(df$X[i+1] - df$X[i]) #calculate the area of each reimann rectangle
    s <- s + ar #updating the reimann area into the sum
  }
  }
  if(type == 'left'){
    for (i in 2:length(df$X)) {
      ar <- df$Y[i]*(df$X[i]-df$X[i-1]) #calculate the area of each reimann rectangle
      s <- s + ar #updating the reimann area into the sum
    }
  }
  if(type == 'avg'){
    LL <- xmin
    UL <- xmax
    sL <- num_integrate(X,Y,xmin=LL, xmax=UL, type='left')
    sR <- num_integrate(X,Y,xmin=LL, xmax=UL, type='right')
    s <- 0.5*(sL + sR) #averaging over the left and right integrals
  }
  return(s)
}
