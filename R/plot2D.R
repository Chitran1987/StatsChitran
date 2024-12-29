##write a function to accept a 2D function f(x,y) along with x and y vectors and plot it
plot2D <- function(X, Y, func, pl = T, gray.dens = 10^5, x.lab = NULL, y.lab = NULL){

  ##error checking####################################################
  ##check whether X, Y is a numeric vector and func is a function
  if(is.numeric(X) != T){
    stop('X should be a numeric vector')
  }
  if(is.numeric(Y) != T){
    stop('Y should be numeric')
  }
  if(is.function(func) != T){
    stop('func should be a function')
  }
  ######################################################################

  ##build the matrix to be plotted######################################
  Z <- outer(X, Y, FUN = "func") #There might be a problem with this line

  ##correct the Z matrix for outputs
  #first transpose and then flip rows
  Z <- t(Z)
  Z <- Z[ nrow(Z):1, ]
  #######################################################################

  ##plot it if pl == T###################################################
  if(pl == F){
    return(Z)
  }else{
    ##correct the Z matrix for the image
    Z1 <- Z #push Z into another container matrix Z1 since you need to output Z
    Z1 <- t(Z1)
    Z1 <- Z1[, ncol(Z1):1]

    #define the grayscale colour
    graycol <- gray(seq(0,1, length.out = gray.dens))
    image(X, Y, Z1, asp = 1.0, col = graycol, xlab = x.lab, ylab = y.lab, useRaster = T)

    #return the actual matrix
    return(Z)
  }
  #########################################################################
}
