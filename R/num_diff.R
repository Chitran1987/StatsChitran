num_diff <- function(dframe, pl=T){
  x <-dframe$V1
  y <-dframe$V2
  x1 <- NA
  y1 <- NA
  for (i in 1:(length(x)-1)) {
    x1 <- c(x1,(x[i+1]+x[i])/2)
    y1 <- c(y1,(y[i+1]-y[i])/(x[i+1]-x[i]))
  }
  x2 <- x1[!is.na(x1)]
  y2 <- y1[!is.na(y1)]

  if(pl==T){
    plot(x2,y2, pch=19, col=rgb(0,0,1,0.3))
  }
  df_res <- data.frame(matrix(c(x2,y2), ncol = 2, byrow = F))
  names(df_res) <- c('X','Y')
  rm(pl)
  return(df_res)
}
