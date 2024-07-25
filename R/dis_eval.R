#calculate the distance between two given points
dis_eval <- function(v1, v2){
  dist <- sqrt( (v2[1] - v1[1])^2 + (v2[2] - v1[2])^2 )
  return(dist)
}
