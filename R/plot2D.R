##write a function to accept a 2D function f(x,y) along with x and y vectors and plot it
plot2D <- function(X, Y, func, pl = T, gray.dens = 10^5, x.lab = NULL, y.lab = NULL){

  ##error checking####################################################
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
  #check whether gray.dens is numeric
  if(err.WN.vec.dim.bit(gray.dens, 1) != T){
    stop('gray.dens should be a whole number and NOT a vector or fraction')
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
