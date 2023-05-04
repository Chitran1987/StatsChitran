data_reduce <- function(X,Y, fract){
  if((fract - as.integer(fract)) != 0){
    stop('input fract has to be an integer')
  }
  fract <- as.integer(fract)
  len_new <- floor(length(X)/fract)
  if(len_new <= 1){
    stop('Not enough points for given value of fract. Reduce the value of fract!!!')
  }
  ind_new <- seq(1:len_new)
  X_new <- X[fract*ind_new]
  Y_new <- Y[fract*ind_new]
  df_new <- data.frame(X_new, Y_new)
  names(df_new) <- c('X','Y')
  return(df_new)
}

