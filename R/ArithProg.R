#write an arithmetic progression with st as start point, d as the difference and n as no. of points
ArithProg<-function(st=0, n, d){
  ####error handling
  if( !is.numeric(st) || length(st)!=1 ){
    stop('st should be a numeric scalar')
  }
  if( !is.numeric(d) || length(d)!=1 ){
    stop('d should be a numeric scalar')
  }
  if( !is.numeric(n) || length(n)!=1 || n%%1 != 0 || n <= 1){
    stop('n should be an integer scalar greater than one')
  }
  v <- Arith_Prog(st, n, d)
  return(v)
}

