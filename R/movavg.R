movavg <- function(X,Y, bn, fn, ord = 1){
  if( (length(bn) != 1)|(length(fn) != 1) ){
    stop('Inputs to fn and fb should be numeric and single digit')
  }else if(length(X) != length(Y)){
    stop('length of input vectors X and Y need to be the same')
  }else if( ord <= 0 | ord%%1 != 0 ){
    stop('Parmater ord needs to be a positive integer')
  }else{
    #it <- ord -1 #no of iterations of this function
    y_res <- NULL
    for (i in 1:length(Y)) {
      y_res <- c( y_res, sum( Y[max(1,i-bn): min(i+fn, length(Y))] )/( min(i+fn, length(Y)) - max(1, i-bn) + 1 ) )
    }
  }
  df <- data.frame(X,y_res)
  names(df) <- c('X','Y')
  if(ord == 1){
    return(df)
  }else{
    df <- movavg(df$X, df$Y, bn, fn, ord = ord - 1)
    return(df)
  }
}
