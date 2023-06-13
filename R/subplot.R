##subplot minimalistic
subplot <- function(v){
  ##error checking
  err.WN.vec.dim(v,2)
  ##actual
  par(mfrow = v)
  return(NULL)
}
