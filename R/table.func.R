#Write a table function for multiple heights and breakpoints
#vector BP is breakpoints
#vector H is heights (Y axis values)
#vector X is the X-axis values
#No of Heights is always one greater than no of breakpoints
table.func <- function(X, BP, H){
  #error magement########################################
  #No of Heights = No of breakpoints + 1
  N <- length(BP)
  M <- length(H)
  if(M != N+1){
    stop('length of H vector should be one more than length of BP vector')
  }

  #Breakpoints within X range
  if(!all(BP > min(X) & BP < max(X))){
    stop('All elements of BP not within scope of X')
  }

  #sort BP values as increasing
  if(any(BP != sort(BP, decreasing = F))){
    stop('vector BP should be sorted as increasing')
  }
  #error magement########################################

  #core code#############################################
  Y <- rep(1.0, length(X))
  df <- data.frame(X, Y)
  for (k in 1:N) {
    if(k == 1){
      df$Y[df$X <= BP[1]] <- H[1]

    }else{
      df$Y[df$X <= BP[k] & df$X > BP[k-1]] <- H[k]
    }
  }
  df$Y[df$X > BP[N]] <- H[N+1]
  #core code#############################################

  #arrange return########################################
  return(df)
  #arrange return########################################
}
