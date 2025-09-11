##check if the input arguments are atomic vectors of a certain dimension
err.vec.dim <- function(v, n){
  if(!is.atomic(v) || !is.vector(v)){
    stop('argument has to be an atomic vector')
  }
  if(length(v) != n){
    stop(paste('dimension of input vector needs to be', n))
  }
  return(NULL)
}
