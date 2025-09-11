##write a digital function with zero or one for specific locations
digi <- function(X, win_df, inv = F){
  #error checking
  if(is.data.frame(win_df) == F){
    stop('win_df argument has to be a dataframe')
  }
  if(dim(win_df)[2] != 2){
    stop('win_df has to be a 2 column dataframe')
  }
  for (i in 1:dim(win_df)[1]) {
    if(win_df$max[i] < win_df$min[i]){
      stop('max values in win_df should be greater than corresponding min values in win_df')
    }
    if(win_df$max[i] > max(X)){
      stop('Max limit of an entry of win_df is greater than the greatest provided X value')
    }
    if(win_df$min[i] < min(X)){
      stop('Min limit of an entry of win_df is less than the least provided X value')
    }
  }
  if(inv == F){
    Y <- rep(1, times = length(X))
    df <- data.frame(X, Y)
    names(df) <- c('X', 'Y')
    for (i in 1:dim(win_df)[1]) {
      df$Y[ (df$X >= win_df$min[i]) & (df$X <= win_df$max[i]) ] <- 0
    }
  }else{
    Y <- rep(0, times = length(X))
    df <- data.frame(X, Y)
    names(df) <- c('X', 'Y')
    for (i in 1:dim(win_df)[1]) {
      df$Y[ (df$X >= win_df$min[i]) & (df$X <= win_df$max[i]) ] <- 1
    }
  }
  return(df)
}
