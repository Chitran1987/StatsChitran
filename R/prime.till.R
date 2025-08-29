#Write a function to create prime nos till N
prime.till <- function(N){
  #error checking
  if( !(N%%1 == 0) | (length(N) != 1) | ( N < 1 ) ){
    stop('N has to be a single and positive integer')
  }
  if(N >= 10^6){
    res <- as.logical(is_prime_par(N))
  }else{
    res <- is.prime(seq(1,N))
  }
  return(res)
}


