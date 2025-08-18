##Write a function to build an ArithGeom prog

ArithGeomProg <- function(st=1, d, r, n){
  Arith.v <- ArithProg(st, n, d)
  Geom.v <- GeomProg(st=1, r, n)
  return(Arith.v*Geom.v)
}

