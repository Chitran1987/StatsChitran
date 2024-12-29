#plot2D.mat used to plot a matrix from x-values, y-values and z-vales
plot2D.mat <- function(X, Y, Z, gray.dens = 10^5, x.lab = NULL, y.lab = NULL){

  ##error checking###########################################################
  #check whether X, Y is a numeric vector and Z is a matrix
  if(is.numeric(X) != T){
    stop('X should be a numeric vector')
  }
  if(is.numeric(Y) != T){
    stop('Y should be numeric')
  }
  if(is.matrix(Z) != T){
    stop('Z has to be a matrix')
  }
  #check whether gray.dens is numeric
  if(err.WN.vec.dim.bit(gray.dens, 1) != F){
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
  #check whether the lengths of X and Y match the no. of columns and rows
  #of the Z matrix
  if(length(X) != ncol(Z)){
    stop('The length of the X vector should be equal to the no. of columns of the Z matrix')
  }
  if(length(Y) != nrow(Z)){
    stop('The length of the Y vector should be equal to the no. of rows of the Z matrix')
  }
  ###########################################################################

  ##core#####################################################################
  #correct the Z matrix for the image function
  Z1 <- Z #push Z into another container matrix Z1 since you need to output Z
  rm(Z)
  Z1 <- t(Z1)
  Z1 <- Z1[, ncol(Z1):1]

  #define the grayscale colour and plot
  graycol <- gray(seq(0,1, length.out = gray.dens))
  image(X, Y, Z1, asp = 1.0, col = graycol, xlab = x.lab, ylab = y.lab, useRaster = T)

  return(NULL)
}
