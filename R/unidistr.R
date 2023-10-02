#write a program to set a likelihood estimation from a uniform distribution
unidistr <- function(X, a, b){
  if(a >= b){
    stop('a should be less than b')
  }else{
    Y <- rep(NA, length(X))
    df <- data.frame(X,Y)
    names(df) <- c('X', 'Y')
    df$Y[df$X < a] <- 0
    df$Y[df$X > b] <- 0
    df$Y[is.na(df$Y) == T] <- 1/(b - a)
  }
  return(df$Y)
}
