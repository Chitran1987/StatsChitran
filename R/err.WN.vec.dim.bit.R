##check if the input arguments are atomic vectors with whole numbers. Returns False if they are
err.WN.vec.dim.bit <- function(v, n){
  if(!is.atomic(v) || !is.vector(v)){
    return(T)
  }
  if(!all(v%%1 == 0)){
    return(T)
  }
  if(length(v) != n){
    return(T)
  }
  return(F)
}
