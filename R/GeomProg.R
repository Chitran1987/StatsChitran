#write a geometric progression with st as start point, r as the difference and n as no. of points
GeomProg<-function(st=1,r,n){
  v <- seq(0, n-1)
  v <- st*r^v
  return(v)
}
