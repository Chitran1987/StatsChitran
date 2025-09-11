######create a function for plotting 3-slice arrays
plot2D.arr <- function(arr, gray.dens = 10^5, x.lab = NULL, y.lab = NULL){
  #check that it is actually a 3 slice array.
  #1) length(dim(arr)) = 3
  if( length(dim(arr)) != 3 ){
    stop('"arr" needs to be a rank-3 array')
  }
  #2) dim(arr)[3] == 3
  if( dim(arr)[3] != 3 ){
    stop('"arr" needs to be a 3-slice rank-3 array. i.e. dim(arr)[3] should be equal to 3')
  }
  #3) arr[,,2] should be an X-matrix
  if( dim( unique( arr[,,2]) )[1] != 1 ){
    stop('"arr[,,2]" should be an X-matrix')
  }
  #4) arr[,,3] should be a Y-matrix
  if( dim( unique( t(arr[,,3]) ) )[1] != 1 ){
    stop('"arr[,,3]" should be an Y-matrix')
  }
  #5) gray.dens should be a numeric scalar
  if( !(is.numeric(gray.dens)) | ( length(gray.dens) != 1 ) ){
    stop('"gray.dens" has to be a numeric scalar')
  }
  #check whether x.lab is NULL or a character of length = 1
  if(is.null(x.lab) != T){
    if( ( (is.character(x.lab)) != T ) | (length(x.lab) != 1 ) ){
      stop('x.lab should be a character vector of length equal to 1')
    }
  }
  #check whether y.lab is NULL or a character of length = 1
  if(is.null(y.lab) != T){
    if( ( (is.character(y.lab)) != T ) | (length(y.lab) != 1 ) ){
      stop('x.lab should be a character vector of length equal to 1')
    }
  }
  #code
  plot2D.mat(X = arr[1,,2], Y = rev(arr[,1,3]), Z = arr[,,1], gray.dens = gray.dens, x.lab = x.lab, y.lab = y.lab)
}
