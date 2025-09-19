#write a geometric progression with st as start point, r as the difference and n as no. of points
GeomProg<-function(st=1,r,n){
  ####error handling
  if( !is.numeric(st) || length(st)!=1 ){
    stop('st should be a numeric scalar')
  }
  if( !is.numeric(r) || length(r)!=1 ){
    stop('d should be a numeric scalar')
  }
  if( !is.numeric(n) || length(n)!=1 || n%%1 != 0 || n <= 1){
    stop('n should be an integer scalar greater than one')
  }

  return(Geom_Prog(st, r, n))
}

