#write a function to normalize a vector
nrm <- function(X, min=0, max=1){
  Y <- ((max - min)/(max(X) - min(X)))*(X - min(X)) + min
  return(Y)
}
