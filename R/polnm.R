##write a program to define a polynomial of nth degree
polnm <- function(X, v){
  if( (is.numeric(X) == F) | (is.numeric(v) == F) ){
    stop('Both vectors X and v should be numeric')
  }
  Y <- 0
  for (k in 1:length(v)) {
    Y <- Y + v[k]*X^(k - 1)
  }
  df <- data.frame(X, Y)
  names(df) <- c('X', 'Y')
  return(df)
}
