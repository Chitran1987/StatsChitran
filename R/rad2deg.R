#write a code to convert radians to degrees
rad2deg <- function(rad, overlap = F){
  if( is.numeric(rad) == F ){
    stop('input must be a numeric vector')
  }
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
