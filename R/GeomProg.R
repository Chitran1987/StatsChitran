#write a geometric progression with st as start point, r as the difference and n as no. of points
GeomProg<-function(st=1,r,n){
  v<-vector(mode = 'numeric', length = n)
  for (i in 1:n) {
    v[i]<-st*(r^(i-1))
  }
  return(v)
}
