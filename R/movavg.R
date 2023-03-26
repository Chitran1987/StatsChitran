movavg <- function(X,Y, bn, fn){
  if( (length(bn) != 1)|(length(fn) != 1) ){
    stop('Inputs to fn and fb should be numeric and single digit')
  }else{
    y_tmp <- c(rep(0,bn), Y, rep(0,fn))
    y_res <- NULL
    for (i in (bn+1):(bn+length(Y))) {
      y_tmp2 <- mean(y_tmp[(i-bn):(i+fn)])
      y_res <- c(y_res, y_tmp2)
    }
    df <- data.frame(X,y_res)
    names(df) <- c('X','Y')
    return(df)
  }
}
