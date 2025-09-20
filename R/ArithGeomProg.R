##Write a function to build an ArithGeom prog

ArithGeomProg <- function(st=1, d, r, n){
  ###Error Checking##############
  if(!is.numeric(st) || length(st)!=1 ){
    stop('argument st should be a numeric scalar')
  }
  if(!is.numeric(d) || length(d)!=1 ){
    stop('argument d should be a numeric scalar')
  }
  if(!is.numeric(r) || length(r)!=1){
    stop('argument r should be a numeric scalar')
  }
  if(!is.numeric(n) || length(n)!=1 || n%%1!=0 || n<=1){
    stop('argument n should be a positive integer scalar greater than one')
  }
  return(Arith_Geom_Prog(st, d, r, n))
}

