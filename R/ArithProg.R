#write an arithmetic progression with st as start point, d as the difference and n as no. of points
ArithProg<-function(st=0, n, d){
  v <- seq(from = st, by = d, length.out = n)
  return(v)
}

