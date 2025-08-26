#Write a script to find the first N prime nos
prime.firstN <- function(N){
  if( !(N%%1 == 0) | (length(N) != 1) | ( N < 1 ) ){
    stop('N has to be a single and positive integer')
  }
  return(prime_firstN(N))
}
