#Create a function that takes in a 2D function 'func' and takes X and Y sequences and buils a 3-slice rank-3 array out of it.
func_2_arr <- function(func, X, Y){
  ###error check######################################
  #func should be a function
  if(!is.function(func)){
    stop('"func" should be a function')
  }
  #X and Y should be numeric
  if( !is.numeric(X) | !is.numeric(Y)){
    stop('"X" and "Y" should be numeric vectors')
  }
  #X and Y should be atomic
  if(!is.atomic(X) | !is.atomic(Y)){
    stop('"X" and Y should be atomic vectors')
  }

  ###code######################################
  ###The actual matrix, M######################
  M <- function_2D(X = X, Y = Y, func = func)
  ####define the function that returns x and ignores y
  fx <- function(x, y){
    return(x)
  }
  ####define the function that returns y and ignores x
  fy <- function(x, y){
    return(y)
  }
  ###return the Mx and My matrices
  Mx <- function_2D(X = X, Y = Y, func = fx)
  My <- function_2D(X = X, Y = Y, func = fy)
  ###define the return array
  arr.ret <- array(NA, dim = c(dim(M), 3) )
  ###fill out the return array
  arr.ret[,,1] <- M
  arr.ret[,,2] <- Mx
  arr.ret[,,3] <- My
  return(arr.ret)
}
