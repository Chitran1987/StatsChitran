##Write a function for computing which elements in a LARGE list contains prime nos
is.prime.large <- function(vec){
  ###error check
  if(!is.numeric(vec)){
    stop('vec has to be a numeric vector')
  }
  if(!all(vec%%1 == 0)){
    stop('vec can contain only positive integers')
  }
  if(!all(vec > 0)){
    stop('vec can only have positive integers')
  }
  if(length(vec) < 10^7){
    return(is_prime(vec))
  }else{
    return(as.logical(is_prime_large(vec)))
  }
}
