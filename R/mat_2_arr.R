#write a program for taking a grayscale Matrix along with X and Y values in a rank-3 array,A_{m, n, p}.
# m X n is the size of the grayscale matrix
# p can take values of 1, 2 or 3. p = 1 is the grayscale matrix. p = 2 is the x-values. p = 3 are the y-values
mat_2_arr <- function(mat, x.lim, y.lim){
  ####error checking
  ##mat is a matrix
  if(!is.matrix(mat)){
    stop("mat has to be a matrix")
  }
  ##x.lim has to be a 2-element numeric vector
  if(!is.vector(x.lim) | !is.atomic(x.lim) | !is.numeric(x.lim) | ( length(x.lim) != 2 ) ){
    stop("x.lim has to be a numeric vector of length = 2")
  }
  ##y.lim has to be a 2-element numeric vector
  if(!is.vector(y.lim) | !is.atomic(y.lim) | !is.numeric(y.lim) | ( length(y.lim) != 2 ) ){
    stop("y.lim has to be a numeric vector of length = 2")
  }
  ##x.lim = c(xmin, xmax)
  if(x.lim[1] > x.lim[2]){
    stop("x.lim[1] needs to be xmin and x.lim[2] needs to be xmax")
  }
  ##y.lim = c(ymin, ymax)
  if(y.lim[1] > y.lim[2]){
    stop("y.lim[1] needs to be ymin and y.lim[2] needs to be ymax")
  }



  ##################

  ####code##########
  ##function to return x
  fx <- function(x, y){
    return(x)
  }
  ##function to return y
  fy <- function(x, y){
    return(y)
  }
  ##define x-sequence and y-sequence
  X <- seq(x.lim[1], x.lim[2], length.out = dim(mat)[2])
  Y <- seq(y.lim[1], y.lim[2], length.out = dim(mat)[1])
  ##define the X-matrix and the Y-matrix
  Mx <- function_2D(X = X, Y = Y, func = fx)
  My <- function_2D(X = X, Y = Y, func = fy)
  ##define the array for the return type and supply the relevant matrices to it
  arr <- array(NA, dim = c(dim(mat), 3) )
  arr[,,1] <- mat
  arr[,,2] <- Mx
  arr[,,3] <- My
  return(arr)

}
