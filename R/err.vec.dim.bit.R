##check if the input arguments are atomic vectors of a certain dimension. Rteurns FALSE if they are
err.vec.dim.bit <- function(v, n){
  if(!is.atomic(v) || !is.vector(v)){
    return(T)
  }
  if(length(v) != n){
    return(T)
  }
  return(F)
}
