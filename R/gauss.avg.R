###Write a program for gaussian kernel smoothening
gauss.avg <- function(X, Y, bn, fn, sig = 1, ord = 1, pl = T){
  #import the libraries
  library(StatsChitran)
  #error checking

  #build the gaussian function
  gauss.h <- function(X){
    return(gauss(X, sig = sig, probability = T))
  }

  #Loop through code for gauss.avg
  for (k in 1:ord) {
    if(i == 1){
      df <- suppressWarnings(movavgf(X, Y, bn, fn, f = gauss.h, val = 0))
    }else{
      df <- suppressWarnings(movavgf(X = df$X, Y = df$Y, bn, fn, f = gauss.h, val = 0))
    }
  }
  if(pl){
    plot(X, Y)
    lines(df$X, df$Y, col = 'red')
    return(df)
  }else{
    return(df)
  }
}
