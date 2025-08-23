####Write a program to return the probability of accumulating ATLEAST a certain no of successes(r) when facing N -trials
prob.success <- function( N.trials, p, r){
  ###error check
  if( (N.trials%%1 != 0) | ( length(N.trials) != 1 ) ){
    stop('N.trials needs to be a single integer')
  }
  if( (p < 0) | (p > 1) | ( length(p) != 1 ) ){
    stop('The probability of a single event, p, should be a numeric value between 0 and 1 ')
  }
  if( (r%%1 != 0) | (r < 0) | (r > N.trials) ){
    stop('n.successes has to be an integer between 0 and N.trials')
  }

  ###code
  n <- N.trials
  pf <- 1- p ###probability of failure
  if(r == 0){
    return(1)
  }else if (r <= n/2){
    k <- seq(0, r-1)
    p.binom <- choose(n, k)*(p^k)*((1 - p)^(n-k))
    pf <- sum(p.binom) #probability of failing at this task
    ps <- 1- pf
    return(ps)
  }else{
    k <- seq(r, n)
    p.binom <- choose(n, k)*(p^k)*((1 - p)^(n-k))
    ps <- sum(p.binom)
    return(ps)
  }

}


