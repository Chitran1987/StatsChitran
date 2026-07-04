###Build a function for plotting a line stacked graph
stack.plot <- function(gr.data, stack.len, stack.rat = 1, col.eq = TRUE,
                       color.mat, type.mat, pch.mat, lwd.mat, cex.mat,
                       main.txt = NULL, axlab = c('', ''), x.lim,
                       show.y.axis = FALSE){

  ##error handling##############################################################

  #gr.data should be present
  if(missing(gr.data) == TRUE){
    stop('gr.data is a mandatory argument')
  }

  #stack.len is mandatory
  if(missing(stack.len) == TRUE){
    stop('stack.len is a mandatory argument')
  }

  #stack.length needs to be a single integer
  b1 = err.WN.vec.dim.bit(stack.len, 1)
  if(b1 == TRUE){
    stop('stack.len needs to be a single integer input')
  }

  #stack.len should be greater than 1
  if(stack.len < 2){
    stop('Use plot() function for graphs with stack.len = 1')
  }

  #stack.rat should be a numeric scalar
  if(!is.numeric(stack.rat) || length(stack.rat) != 1 || is.na(stack.rat)){
    stop('stack.rat should be a numeric scalar')
  }

  #stack.rat should be non-negative
  if(stack.rat < 0){
    stop('stack.rat should be non-negative')
  }

  #col.eq should be a boolean scalar
  if(!is.logical(col.eq) || length(col.eq) != 1 || is.na(col.eq)){
    stop('col.eq should be a boolean scalar')
  }

  #length of gr.data should be equal to stack length
  if(length(gr.data) != stack.len){
    stop('length of gr.data should be equal to stack length')
  }

  #each element of the list should be a dataframe
  for (i in 1:stack.len) {
    if(is.data.frame(gr.data[[i]]) == FALSE){
      stop('each element of the list should be a dataframe')
    }
  }

  #column dimension of each dataframe should be even
  for (i in 1:stack.len) {
    if(dim(gr.data[[i]])[2] %% 2 != 0){
      stop('Each dataframe should have an even no. of columns')
    }
  }

  #each dataframe should have equal no. of columns if col.eq is true
  if(col.eq){
    a <- dim(gr.data[[1]])[2]

    for (i in 2:stack.len) {
      if(dim(gr.data[[i]])[2] != a){
        stop('All dataframes should have equal no. of columns since col.eq = TRUE')
      }
    }

    n <- a/2

  }else{
    a <- NULL

    for (i in 1:stack.len) {
      a <- c(a, dim(gr.data[[i]])[2])
    }

    n <- max(a)/2 #no. of graphs in a single level of the stack
  }

  #if color.mat is not provided, default to black
  if(missing(color.mat) == TRUE){
    color.mat <- matrix(data = rep('black', times = stack.len*n), ncol = n)
  }

  #color.mat needs to be a matrix
  if(is.matrix(color.mat) == FALSE){
    stop('color.mat needs to be of type matrix')
  }

  #dimensions of color.mat needs to be (stack.len X n)
  if((dim(color.mat)[1] != stack.len) | (dim(color.mat)[2] != n)){
    stop('dimension of color.mat must be (*stack.len* X *max no. of graphs on a single level*)')
  }

  #dimension of type.mat needs to be (stack.len X n)
  #if not given default to line
  if(missing(type.mat) == TRUE){
    type.mat <- matrix(data = rep('l', times = stack.len*n), ncol = n)
  }else{
    if(is.matrix(type.mat) == FALSE){
      stop('type.mat needs to be of type matrix')
    }

    if((dim(type.mat)[1] != stack.len) | (dim(type.mat)[2] != n)){
      stop('no. of rows and columns of type.mat argument should be equal to stack.len and n respectively')
    }
  }

  #dimension of pch.mat needs to be (stack.len X n)
  #if not given use default
  if(missing(pch.mat) == TRUE){
    pch.mat <- matrix(data = rep(1, times = stack.len*n), ncol = n)
  }else{
    if(is.matrix(pch.mat) == FALSE){
      stop('pch.mat needs to be of type matrix')
    }

    if((dim(pch.mat)[1] != stack.len) | (dim(pch.mat)[2] != n)){
      stop('no. of rows and columns of pch.mat argument should be equal to stack.len and n respectively')
    }
  }

  #dimension of cex.mat needs to be (stack.len X n)
  #if not given use default
  if(missing(cex.mat) == TRUE){
    mat_cex <- matrix(rep(1, times = stack.len*n), nrow = stack.len, ncol = n)
  }else{
    if(is.matrix(cex.mat) == FALSE){
      stop('cex.mat needs to be of type matrix')
    }

    if((dim(cex.mat)[1] != stack.len) | (dim(cex.mat)[2] != n)){
      stop('no. of rows and columns of cex.mat argument should be equal to stack.len and n respectively')
    }

    mat_cex <- cex.mat
  }

  #dimension of lwd.mat needs to be (stack.len X n)
  #if not given use default
  if(missing(lwd.mat) == TRUE){
    lwd.mat <- matrix(data = rep(1, times = stack.len*n), ncol = n)
  }else{
    if(is.matrix(lwd.mat) == FALSE){
      stop('lwd.mat needs to be of type matrix')
    }

    if((dim(lwd.mat)[1] != stack.len) | (dim(lwd.mat)[2] != n)){
      stop('no. of rows and columns of lwd.mat argument should be equal to stack.len and n respectively')
    }
  }

  #x.lim should be a length 2 numeric vector
  #min x.lim should be less than max of x.lim
  if(!missing(x.lim)){
    if(!is.numeric(x.lim) || length(x.lim) != 2){
      stop('x.lim should be a numeric vector of length 2')
    }

    if(x.lim[1] >= x.lim[2]){
      stop('x.lim[1] should be the lower value within the range')
    }
  }

  #show.y.axis should be a boolean scalar
  if(!is.logical(show.y.axis) || length(show.y.axis) != 1 || is.na(show.y.axis)){
    stop('show.y.axis should be a boolean scalar')
  }

  #########################################################################################################


  ##actual code############################################################################################

  L <- gr.data

  #figure out xmax and xmin for entire stack
  if(missing(x.lim)){
    X <- NULL

    for (i in 1:stack.len) {
      for (j in 1:dim(L[[i]])[2]) {
        if(j %% 2 == 1){
          X <- c(X, L[[i]][, j])
        }
      }
    }

    xmax <- max(X)
    xmin <- min(X)

  }else{
    xmax <- x.lim[2]
    xmin <- x.lim[1]
  }


  #figure out the ymax and ymin for entire stack
  Y_span <- NULL #A vector containing the heights of each individual level of the stack
  Y_min <- NULL  #A vector containing the minimum of each individual level of the stack

  for (i in 1:stack.len) {
    Y <- NULL

    for (j in 1:dim(L[[i]])[2]) {
      if(j %% 2 == 0){
        dt <- L[[i]][, j]
        Y <- c(Y, dt)
      }
    }

    Y_span <- c(Y_span, max(Y) - min(Y))
    Y_min <- c(Y_min, min(Y))
  }

  #vertical offset of each stack level
  Y_offset <- c(0, stack.rat * cumsum(Y_span[1:(stack.len - 1)]))

  ymin <- 0
  ymax <- max(Y_offset + Y_span)

  #avoid zero-height plotting window
  if(ymax == ymin){
    ymax <- ymin + 1
  }

  #check if show.y.axis is a boolean argument
  if(show.y.axis){
    y.bit <- 's'
  }else{
    y.bit <- 'n'
  }

  #start plotting the actual data
  plot(NA, NA,
       xlim = c(xmin, xmax),
       ylim = c(ymin, ymax),
       yaxt = y.bit,
       xlab = axlab[1],
       ylab = axlab[2],
       main = main.txt)

  for (i in 1:stack.len) {
    df <- L[[i]]
    jlim <- dim(df)[2]/2

    for (j in 1:jlim) {
      lines(df[, 2*j - 1],
            df[, 2*j] - Y_min[i] + Y_offset[i],
            type = type.mat[i, j],
            lwd = lwd.mat[i, j],
            pch = pch.mat[i, j],
            cex = mat_cex[i, j],
            col = color.mat[i, j])
    }
  }

  return(NULL)
}
