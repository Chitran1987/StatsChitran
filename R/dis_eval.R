#calculate the distance between two given points
dis_eval <- function(v1, v2){
  #error handling
  if(length(v1) != length(v2)){
    stop('numeric vectors should have equal lengths')
  }
  Del_vec <- v1 - v2
  dist <- sqrt( sum(Del_vec^2) )
  return(dist)
}
