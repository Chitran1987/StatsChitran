num_diff <- function(X,Y, type='center', pl=T, order=1){
  ##error handling here#################################
  if(length(X) != length(Y)){
    stop('lengths of X and Y not equal') ##check for lengths
  }
  if((is.numeric(X)&is.numeric(Y)) == F){
    stop('Either X or Y is not numeric') ##check if numeric(answers F for list or dataframe )
  }
  if(length(order) != 1){
    stop('order argument should be a scalar') ##check if order argument is a scalar
  }
  if(is.numeric(order) == F){
    stop('order argument should be a numeric') ##check if the order argument is numeric
  }
  if(ceiling(order) > order){
    stop('The order argument should be an integer') ##check if the order argument is an integer
  }
  if(order < 1){
    stop('order srgument should be an integer greater than or equal to 1') ##check if the order argument is an integer greater than or equal to 1
  }
  ##execution starts here#################################
  X0 <- X
  Y0 <- Y
  n <- order

  while(n > 0){
    Y1 <- diff(Y0)/diff(X0) #calculate the usual numeric difference of Y divided by the difference of X

    if(type == 'left'){
      X1 <- X0[-1] #remove first element of X
    }
    if(type == 'right'){
      X1 <- X0[-length(X0)] #remove the last element of X
    }
    if(type == 'center'){
      X1 <- NULL
      for (i in 1:(length(X0)-1)) {
        X1 <- c(X1, (X0[i+1] + X0[i])/2) #average the list of values of X
      }
    }
    X0 <- X1
    Y0 <- Y1
    n <- n - 1
  }

  df_res <- data.frame(X1,Y1)
  names(df_res) <- c('X','Y')
  if(pl==T){
    #define the plot Y-limits, since X-limits will be the same
    ymin <- min(c(Y,Y1))
    ymax <- max(c(Y,Y1))
    plot(X,Y, pch =19, col=rgb(0,0,1,0.3), type='l', ylim = c(ymin, ymax))
    lines(X1,Y1, col=rgb(1,0,0))
    legend('topright', legend = c('original', paste('derivative', '(', as.character(order), ')')), col = c(rgb(0,0,1,0.3), rgb(1,0,0)), bg='transparent', box.lty = 0, lty =1 )
  }
  #rm(pl)
  return(df_res)
}

