##check if the input arguments are atomic vectors with whole numbers
err.WN.vec.dim <- function(v, n){
  if(!is.atomic(v) || !is.vector(v)){
    stop('argument has to be an atomic vector')
  }
  if(!all(v%%1 == 0)){
    stop('argument has to be an integer vector')
  }
  if(length(v) != n){
    stop(paste('dimension of input vector needs to be', n))
  }
  return(NULL)
}

