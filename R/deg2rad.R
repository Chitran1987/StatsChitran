#function for converting degrees to radians
deg2rad <- function(deg, overlap=F){
  ###error checking
  #if deg is numeric
  if( is.numeric(deg) == F ){
    stop('argument "deg" must be a numeric vector')
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
    return((pi/180)*deg)
  }else{
    bin_tab <- deg >= 0
    res_vec <- vector(mode = 'numeric', length = length(bin_tab))
    for (i in 1:length(bin_tab)) {
      if( bin_tab[i] == T){
        a <- (pi/180)*deg[i]
        rem <- a%%pi
        quo <- floor(a/pi)
        if(quo%%2 == 0){
          res_vec[i] <- rem
        }else{
          res_vec[i] <- rem - pi
        }

      }else{
        a <- (pi/180)*deg[i]
        a <- abs(a)
        rem <- a%%pi
        quo <- floor(a/pi)
        if(quo%%2 == 0){
          res_vec[i] <- -1*rem
        }else{
          res_vec[i] <- -1*(rem - pi)
        }
      }
    }
  }
  return(res_vec)
}

