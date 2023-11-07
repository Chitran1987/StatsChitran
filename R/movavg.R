movavg <- function(X,Y, bn, fn){
  if( (length(bn) != 1)|(length(fn) != 1) ){
    stop('Inputs to fn and fb should be numeric and single digit')
  }else if(length(X) != length(Y)){
    stop('length of input vectors X and Y need to be the same')
  }else{
    y_res <- NULL
    for (i in 1:length(Y)) {
      y_res <- c( y_res, sum( Y[max(1,i-bn): min(i+fn, length(Y))] )/( min(i+fn, length(Y)) - max(1, i-bn) + 1 ) )
    }
  }
  df <- data.frame(X,y_res)
  names(df) <- c('X','Y')
  return(df)
}
