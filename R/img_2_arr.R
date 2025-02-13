#write a program for taking a image along with X and Y limits and converting it into  a rank-3 array,A_{m, n, p}.
# m X n is the size of the grayscale matrix
# p can take values of 1, 2 or 3. p = 1 is the grayscale matrix. p = 2 is the x-values. p = 3 are the y-values of each point in the grid
img_2_arr <- function(source.png, x.lim, y.lim, pl = T, bgrnd = 1){

  #Call img_2_dat and work on it
  L_dmp <- img_2_data(source.png = source.png, x.lim = x.lim, y.lim = y.lim, pl = pl, bgrnd = bgrnd)
  M <- L_dmp$gray.val
  X <- L_dmp$data$X
  Y <- L_dmp$data$Y
  rm(L_dmp)

  #define the values of the X and Y co-ordinate matrixes of the (X, Y) grid
  fx_dumm <- function(X, Y){
    return(X)
  }
  fy_dumm <- function(X, Y){
    return(Y)
  }
  Mx <- function_2D(X, Y, func = fx_dumm)
  My <- function_2D(X, Y, func = fy_dumm)
  gray.arr <- array(data = NA, dim = c(dim(M), 3))
  gray.arr[, , 1] <- M
  gray.arr[, , 2] <- Mx
  gray.arr[, , 3] <- My
  return(gray.arr)
}
