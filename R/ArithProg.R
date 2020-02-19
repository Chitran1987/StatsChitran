#write an arithmetic progression with st as start point, d as the difference and n as no. of points
ArithProg<-function(st=0, n, d){
  v<-vector(mode = 'numeric', length = n)
  for (i in 1:n) {
    v[i]<-st+(i-1)*d
  }
  return(v)
}
