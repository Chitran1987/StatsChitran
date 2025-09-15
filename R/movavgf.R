#write a movavgf function to accept another function and use it as a moving window for moving average calculations
movavgf <- function(X,Y, bn, fn, f, val, ord = 1){
  ###error checking
  err.vec.dim(val, 1)
  err.WN.vec.dim(bn, 1)
  err.WN.vec.dim(fn, 1)
  if( (length(bn) != 1)|(length(fn) != 1) ){
    stop('Inputs to fn and fb should be numeric and single digit')
  }else if(length(X) != length(Y)){
    stop('length of input vectors X and Y need to be the same')
  }else if(is.function(f) == F){
    stop('parameter f needs to be a function')
  }else{
    samp <- mean(diff(X)) #The sampling difference
    dev <- sd(diff(X)) #Standard dev. of sampling difference. Only for input to user.
    warning('The sampling mean is ', samp,' while the standard deviation is ', dev, '\n' )
    warning('values of sampling mean will be used to create smoothing function for moving averages', '\n')
    #build the window sequence##############################################
    span1 <- ArithProg(st=val, n = fn+1, d = samp)
    span2 <- ArithProg(st= val - samp, n = bn, d = -samp)
    span <- c(rev(span2), span1)
    if(length(span) != bn + fn + 1){
      stop('Check length of span vector')
    }
    k <- bn + 1 #index value of 'val'
    f_seq <- f(span) #write the function into a sequence
    if(sum(abs(f_seq) == Inf ) != 0){
      stop('Infinities exist in your window function f')
    }
    if(sum(is.na(f_seq)) != 0){
      stop('NAs exist in the window function')
    }
    #Call the cpp function
    Y.res <- kernel_movavgf(Y, bn, fn, w= f_seq, ord = ord)
  }
  Y <- Y.res
  rm(Y.res)
  ###############################################################
  #build and return the smoothed dataframe#######################
  df <- data.frame(X, Y)
  names(df) <- c('X','Y')
  return(df)
  ###############################################################
}


