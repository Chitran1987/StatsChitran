#write a code to convert radians to degrees
rad2deg <- function(rad, overlap = F){
  ###error checking
  #if rad is numeric
  if( is.numeric(rad) == F ){
    stop('argument "rad" must be a numeric vector')
  }
  #if overlap is a vector of dimension 1
  if ( err.vec.dim.bit(overlap, 1) == T ){
    stop('argument "overlap" must have a dimension of 1')
  }
  #if overlap is a boolean bit
  if( is.logical(overlap) == F ){
    stop('argument "overlap" needs to be of type logical')
  }

  ###body
  if(overlap == F){
    return((180/pi)*rad)
  }else{
    bin_tab <- rad >= 0
    res_vec <- vector(mode = 'numeric', length = length(bin_tab))
    for (i in 1:length(bin_tab)) {
      if( bin_tab[i] == T){
        a <- (180/pi)*rad[i]
        rem <- a%%180
        quo <- floor(a/180)
        if(quo%%2 == 0){
          res_vec[i] <- rem
        }else{
          res_vec[i] <- rem - 180
        }

      }else{
        a <- (180/pi)*rad[i]
        a <- abs(a)
        rem <- a%%180
        quo <- floor(a/180)
        if(quo%%2 == 0){
          res_vec[i] <- -1*rem
        }else{
          res_vec[i] <- -1*(rem - 180)
        }
      }
    }

  }
  return(res_vec)
}
