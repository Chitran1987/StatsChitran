#write a program to create a 2D function and return the matrix
function_2D <- function(X, Y, func){
  ##error checking###########################################
  #check whether X, Y is a numeric vector and func is a function
  if(is.numeric(X) != T){
    stop('X should be a numeric vector')
  }
  if(is.numeric(Y) != T){
    stop('Y should be numeric')
  }
  if(is.function(func) != T){
    stop('func should be a function')
  }
  ###########################################################

  ##core#####################################################

  ##build the matrix to be plotted######################################
  Z <- outer(X, Y, FUN = "func") #There might be a problem with this line

  ##correct the Z matrix for outer() function renundancies
  #first transpose and then flip rows
  Z <- t(Z)
  Z <- Z[ nrow(Z):1, ]
  #######################################################################

  return(Z)
}
