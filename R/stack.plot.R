###Build a function for plotting a line stacked graph
stack.plot <- function(gr.data, stack.len, stack.rat = 1, col.eq = T, color.mat, type.mat, pch.mat, lwd.mat, main.txt = NULL, axlab = c('', '')){
  ##error handling##############################################################
  #gr.data should be present
  if(missing(gr.data) == T){
    stop('gr.data is a mandatory argument')
  }
  #stack.len is mandatory
  if(missing(stack.len) == T){
    stop('stack.len is a mandatory argument')
  }
  #stack.length needs to be a single integer
  b1 = err.WN.vec.dim.bit(stack.len, 1)
  if(b1 == T){
    stop('stack.len needs to be a single integer input')
  }
  #length of gr.data should be equal to stack length
  if(length(gr.data) != stack.len){
    stop('length of gr.data should be equal to stack length')
  }
  #each element of the list should be a dataframe
  for (i in 1:stacklen) {
    if(is.data.frame(gr.data[[i]]) == F){
      stop('each element of the list should be a dataframe')
    }
  }
  #column dimension of each dataframe should be even
  for (i in 1:stack.len) {
    if( dim(gr.data[[i]])[2]%%2 != 0 ){
      stop('Each dataframe should have an even no. of columns')
    }
  }
  #each dataframe should have equal no. of columns if col.eq is true
  if(col.eq == T){
    a <- dim(gr.data[[1]])[2]
    for (i in 2:stack.len) {
      if( dim(gr.data[[1]])[2] != a ){
        stop('All dataframes should have equal no. of columns since col.eq = T')
      }
    }
  }else{
    a <- NULL
    for (i in 1:stack.len) {
      a <- c( a, dim(gr.data[[i]])[2] )
    }
    n <- max(a)/2 #no. of graphs in a single level of the stack

  }
  n <- dim(gr.data[[i]])[2]/2
  #if col.mat is not provided, default to black
  if(missing(color.mat) == T){
    color.mat <- matrix(data = rep('black', times = stack.len*n), ncol = n)
  }
  #color.mat needs to be a matrix
  if(is.matrix(color.mat) == F){
    stop('color.mat needs to be of type matrix')
  }
  #dimensions of color.mat needs to be (stack.len X  n)
  if( (dim(color.mat)[1] != stack.len) | (dim(color.mat)[2] != n) ){
    stop('dimension of color.mat must be (*stack.len* X *max no. of graphs on a single level*)')
  }
  #dimesnion of type.mat needs to be (stack.len X n)
  #if not given default to line
  if(missing(type.mat) == T){
    type.mat <- matrix(data = rep('l', times = stack.len*n), ncol = n )
  }else{
    if(is.matrix(type.mat) == F){
      stop('type.mat needs to be of type matrix')
    }
    if( (dim(type.mat)[1] != stack.len) | (dim(type.mat)[2] != n) ){
      stop('no. of rows and columns of type.mat argument should be equal to stack.len and n respectively')
    }
  }
  #dimension of pch.mat needs to be (stack.len x n)
  #if not given use default
  if(missing(pch.mat) == T){
    pch.mat <- matrix(data = rep(1, times = stack.len*n), ncol = n )
  }else{
    if(is.matrix(pch.mat) == F){
      stop('pch.mat needs to be of type matrix')
    }
    if( (dim(pch.mat)[1] != stack.len) | (dim(pch.mat)[2] != n) ){
      stop('no. of rows and columns of pch.mat argument should be equal to stack.len and n respectively')
    }
  }
  #dimension of lwd.mat needs to be (stack.len x n)
  #if not given use default
  if(missing(lwd.mat) == T){
    pch.mat <- matrix(data = rep(1, times = stack.len*n), ncol = n )
  }else{
    if(is.matrix(lwd.mat) == F){
      stop('lwd.mat needs to be of type matrix')
    }
    if( (dim(lwd.mat)[1] != stack.len) | (dim(pch.mat)[2] != n) ){
      stop('no. of rows and columns of lwd.mat argument should be equal to stack.len and n respectively')
    }
  }
  #########################################################################################################


  ##actual code############################################################################################
  L <- gr.data
  #figure out xmax and xmin for entire stack
  X <- NULL
  for (i in 1:stack.len) {
    for (j in 1:dim(L[[i]])[2]) {
      if(j%%2 == 1){
        X <- c(X, L[[i]][,j])
      }
    }
  }
  xmax <- max(X)
  xmin <- min(X)

  #figure out the ymax and ymin for entire stack
  Y_span <- NULL #A vector containing the heights of each individual level of the stack
  Y_min <- NULL #A vector containing the minimum of each individual level of the stack
  for (i in 1:stack.len) {
    Y <- NULL
    for (j in 1:dim(L[[i]])[2]) {
      if(j%%2 == 0){
        dt <- L[[i]][,j]
        Y <- c(Y, dt)
      }

    }
    Y_span <- c(Y_span, max(Y) - min(Y))
    Y_min <- c(Y_min, min(Y))
  }
  stack.ht <- sum(Y_span*stack.rat) #maximum height of the stack
  ymin <- 0
  ymax <- stack.ht



  #start plotting the actual data
  plot(NA, NA, xlim=c(xmin, xmax), ylim=c(ymin, ymax), yaxt = 'n', xlab = axlab[1], ylab = axlab[2], main = main.txt)
  for (i in 1:stack.len) {
    df <- L[[i]]
    jlim <- dim(df)[2]/2
    for ( j in 1:jlim ) {
      if(i == 1){
        lines( df[,2*j -1], df[, 2*j] - Y_min[i] , type = type.mat[i, j], lwd = lwd.mat[i, j], pch = pch.mat[i, j], col = color.mat[i, j] )
      }else{
        lines( df[,2*j -1], df[, 2*j] - Y_min[i] + stack.rat*sum(Y_span[1:i-1]), type = type.mat[i, j], lwd = lwd.mat[i, j], pch = pch.mat[i, j], col = color.mat[i, j] )
      }


    }
  }

  return(NULL)
}
