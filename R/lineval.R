##given two points (x1,y1), (x2,y2) and a dataset X, spill out a vector Y
##Y should give the values of Y on the line corresponding to the given X
lineval <- function(v1, v2, X){
  #v1 and v2 have to be two point vectors
  err.vec.dim(v1,2)
  err.vec.dim(v2,2)
  x1 <- v1[1]
  y1 <- v1[2]
  x2 <- v2[1]
  y2 <- v2[2]
  Y <- ((y2 - y1)/(x2 - x1))*X + (x2*y1 - y2*x1)/(x2 - x1)
  return(Y)
}
