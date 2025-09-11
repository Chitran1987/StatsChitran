###Write a program for gaussian kernel smoothening
gauss.avg <- function(X, Y, bn, fn, sig = 1, ord = 1, pl = T){
  #import the libraries
  library(StatsChitran)
  #error checking

  #build the gaussian function
  gauss.h <- function(v){
    return(gauss(X = v, sig = sig, probability = T))
  }

  #Call the movavgf function
      df <- suppressWarnings(movavgf(X, Y, bn, fn, f = gauss.h, val = 0, ord = ord))
  if(pl){
    plot(X, Y)
    lines(df$X, df$Y, col = 'red')
  }
   return(df)
}
