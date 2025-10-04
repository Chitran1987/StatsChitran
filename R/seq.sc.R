##Write a function to write a sequence just by length
seq.sc <- function(st, end, n){
  if( !is.numeric(st) | length(st)!=1 ){
    stop('st needs to be a numeric scalar')
  }
  if( !is.numeric(end) | length(end)!=1 ){
    stop('end needs to be a numeric scalar')
  }
  if(!is.numeric(n) || length(n)!=1 || n%%1 != 0){
    stop('n needs to be a numeric integer')
  }
  if(n <= 0){
    stop('n needs to be a positive integer scalar')
  }
  if( st >= end){
    stop('Argument st has to be less than end')
  }
  return(seq_sc(st, end, n))


}
