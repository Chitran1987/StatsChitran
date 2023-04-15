num_diff <- function(X,Y, type='center', pl=T){
  ##error handling here#################################
  if(length(X) != length(Y)){
    stop('lengths of X and Y not equal') ##check for lengths
  }
  if((is.numeric(X)&is.numeric(Y)) == F){
    stop('Either X or Y is not numeric') ##check if numeric(answers F for list or dataframe )
  }
  ##execution starts her#################################
  Y1 <- diff(Y)/diff(X) #calculate the usual numeric difference of Y divided by the difference of X

  if(type == 'left'){
    X1 <- X[-1] #remove first element of X
     }
  if(type == 'right'){
    X1 <- X[-length(X)] #remove the last element of X
     }
  if(type == 'center'){
    X1 <- NULL
    for (i in 1:(length(X)-1)) {
      X1 <- c(X1, (X[i+1] + X[i])/2) #average the list of values of X
    }
  }
  df_res <- data.frame(X1,Y1)
  names(df_res) <- c('X','Y')
  if(pl==T){
    plot(X,Y, pch =19, col=rgb(0,0,1,0.3), type='l')
    lines(X1,Y1, col=rgb(1,0,0))
    legend('topright', legend = c('original', 'num_diff'), col = c(rgb(0,0,1,0.3), rgb(1,0,0)), bg='transparent', box.lty = 0, lty =1 )
  }
  #rm(pl)
  return(df_res)
}
