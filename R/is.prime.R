##Write a function to check whether a no. is a prime no. or not
is.prime <- function(vec){


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
  return(is_prime(vec))
}
