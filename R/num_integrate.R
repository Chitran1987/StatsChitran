#Create an approximate numeric integration function
num_integrate <- function(X,Y, xmin, xmax, type='avg'){
  ##error handling
  if(xmin < min(X)){
    stop('Lower bound of integration not in domain')
  }
  if(xmax > max(X)){
    stop('Upper bound of integration not in domain')
  }
  if(length(X) != length(Y)){
    stop('X and Y are not of the same lengths')
  }
  if(!is.numeric(X) | !is.numeric(Y) ){
    stop('X and Y need to be numeric vectors')
  }
  if(xmax == xmin){
    return(0)
  }
  if(abs(xmax - xmin) < 4*mean(diff(X))){
    stop('Not enough samples to measure an integral')
  }
  if(length(X) < 4){
    stop('Not enough samples with probably non-uniform sampling')
  }
  if(length(X) <= 5*10^5){
    s = num_integrate_cpp(X, Y, xmin, xmax)
  }else{
    s = num_integrate_par(X, Y, xmin, xmax)
  }
  return(s)
}
