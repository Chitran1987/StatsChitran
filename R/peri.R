#a function to calculate the perimeter of a 2D curve/function
peri <- function(df, xmin, xmax){
  ##error catching
  if(is.data.frame(df) == F){
    stop('The df argument has to be a data frame')
  }
  if( (is.numeric(xmin) == F) | (is.numeric(xmax) == F) ){
    stop('The xmax and xmin arguments have to be of type numeric')
  }
  if( (err.vec.dim.bit(xmin, 1) != 1) | (err.vec.dim.bit(xmax, 1) != 1) ){
    stop('xmax and xmin have to be vectors of length equal to 1')
  }


  ##core function
  df <- select(df, X >= xmin)
  df <- select(df, X <= xmax)
  L <- length(df$X)
  peri_vec <- NULL
  for (i in 1:(L-1) ) {
    peri_vec <- c( peri_vec, dis_eval(df[i,], df[i+1,]) )
  }
  return(sum(peri_vec))
}
