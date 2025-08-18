###Write a program for gaussian kernel smoothening
gauss.avg <- function(X, Y, bn, fn, sig = 1, ord = 1, pl = T){
  #import the libraries
  library(StatsChitran)
  #error checking

  #build the gaussian function
  gauss.h <- function(X){
    return(gauss(X, sig = sig, probability = T))
  }

  #call the movavgf function
  df <- movavgf(X, Y, bn, fn, f = gauss.h, val = 0)

  #Recursion using ord argument
  if(ord == 1){
    if(pl){
      plot(X, Y)
      lines(df$X, df$Y, col = 'red')
    }
    return(df)
  }else{
    df <- gauss.avg(X = df$X, Y = df$Y, bn, fn, sig, ord = ord-1, pl)
  }
}
