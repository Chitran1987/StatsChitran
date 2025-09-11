##Write a program to periodify an X-Y dataset
#The period is assumed to be equal to the length of the X dataset plus a unit sample
periodify <- function(X, Y, fwd, bwd){
  ##error handling here
  #X is a numeric vector
  len_X <- length(X)
  if( (!( err.vec.dim.bit(X, len_X) )&( is.numeric(X) )) == F ){
    stop('X has to be a numeric vector of the atomic type')
  }
  #Y is a numeric vector
  len_Y <- length(Y)
  if( (!( err.vec.dim.bit(Y, len_Y) )&( is.numeric(Y) )) == F ){
    stop('Y has to be a numeric vector of the atomic type')
  }
  #length(X) = length(Y)
  if(len_X != len_Y){
    stop('X and Y should have the same lengths')
  }
  #fwd and bwd are positive numeric integers
  if( (err.WN.vec.dim.bit(fwd, 1) | err.WN.vec.dim.bit(bwd, 1)) == T ){
    stop('The "fwd" and "bwd" argument have to be single valued whole numbers or integers each')
  }
  #warning: Non uniform sampling rate in X
  if( sum(diff(diff(X))) != 0 ){
    warning('non uniform sampling in X: Mean of sampling error vector will be used to calculate sampling rate')
  }

  ##code here
  L <- vector('list', length = fwd + bwd + 1) #list containing each individual dataset
  L[[bwd + 1]] <- data.frame(X, Y) #put the original dataset in the right position
  s <- mean(diff(X)) #calculate the sampling rate
  span <- max(X) - min(X) #calculate the span

  #calculate the values before and after the current dataset
  for (i in -bwd:fwd) {
    L[[bwd + 1 + i]] <- data.frame( (X + span*i + s*sign(i) ), Y )
    names(L[[bwd + 1 + i]]) <- c('X', 'Y')
  }

  #concatenate all the dataframes
  dmp <- data.frame(1, 1) #garbage value first row
  names(dmp) <- c('X', 'Y')
  for (i in 1:(fwd + bwd + 1)) {
    dmp <- rbind(dmp, L[[i]])
  }
  res <- dmp[-1,] #remove the first garbage value row
  return(res)

}
