#Write a padding function for increasing the no. of data points within the dataset for fourier space analysis
padding <- function(X, Y, fwd, bwd, fval, bval, pad.type = 'default'){

  #error handling
  #pad.type has to be a character vector
  if(is.character(pad.type) == F){
    stop('The input "pad.type" has to be a character input')
  }
  #pad.type has to have a length of one
  if(length(pad.type) != 1){
    stop('The input "pad.type" has to be a single element character vector')
  }
  #vec_pad.type can take values of 'default', 'zero' or 'custom'
  vec_pad.type <- c('default', 'zero', 'custom')
  if( sum(pad.type == vec_pad.type) != 1 ){
    stop('The input "pad.type" can either take values of "default", "zero" or "custom"')
  }
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
  #bval and fval must be provided if pad.type is 'custom' and should not be provided otherwise
  if( pad.type == 'custom'){
    if( missing(fval) | missing(bval) ){
      stop('If "pad.type" is assigned a "custom" value, then fval and bval need to be provided')
    }
  }else{
    if( !missing(fval) | !missing(bval) ){
      stop('If "pad.type" is not assigned a "custom" value, then fval and bval remain unused')
    }
  }
  #warning: Non uniform sampling rate in X
  if( sum(diff(diff(X))) != 0 ){
    warning('non uniform sampling in X: Mean of sampling error vector will be used to calculate sampling rate')
  }

  #code here
  ##code here
  L <- vector('list', length = fwd + bwd + 1) #list containing each individual dataset
  #L[[bwd + 1]] <- data.frame(X, Y) #put the original dataset in the correct position
  s <- mean(diff(X)) #calculate the sampling rate
  span <- max(X) - min(X) #calculate the span

  #calculate the values before and after the current dataset
  if( pad.type == 'zero' ){
    Y1 <- rep(0, times = len_X)
    Y2 <- rep(0, times = len_X)
  }else if( pad. type == 'default' ){
    Y1 <- rep(Y[1], times = len_X )
    Y2 <- rep(Y[len_X] times = len_X)
  }else{
    Y1 <- rep(bval, times = len_Y)
    Y2 <- rep(fval, times = len_Y)
  }

  #put the Y1 based dataframes in the backward location
  for (i in -bwd:-1) {
    L[[bwd + 1 + i]] <- data.frame( (X + span*i + s*sign(i) ), Y1 )
    names(L[[bwd + 1 + i]]) <- c('X', 'Y')
  }

  #put the Y2 based dataframes in the forward position
  for (i in 1:fwd) {
    L[[bwd + 1 + i]] <- data.frame( (X + span*i + s*sign(i) ), Y2 )
    names(L[[bwd + 1 + i]]) <- c('X', 'Y')
  }
  L[[bwd + 1]] <- data.frame(X, Y) #put back the original dataset in the correct position
  names(L[[bwd + 1]]) <- c('X', 'Y')

  #concatenate all the dataframes
  dmp <- data.frame(1, 1) #garbage value first row
  names(dmp) <- c('X', 'Y')
  for (i in 1:(fwd + bwd + 1)) {
    dmp <- rbind(dmp, L[[i]])
  }
  res <- dmp[-1,] #remove the first garbage value row
  return(res)
}
