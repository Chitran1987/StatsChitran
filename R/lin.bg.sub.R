lin.bg.sub <- function(df, win_df, pl = T){
  ##error checking
  #df is a data frame
  if( is.data.frame(df) == F ){
    stop('df has to be a 2 column dataframe')
  }
  # win_df is a dataframe
  if( is.data.frame(win_df) == F ){
    stop('win_df has to be a 2 column dataframe')
  }
  #df is 2 column
  if( dim(df)[2] != 2 ){
    stop('df has to be a 2 column dataframe')
  }
  #names of win_df columns have to be min and max
  if( ( names(win_df)[1] != 'min') | ( names(win_df)[2] != 'max' ) ){
    stop('name of 1st column of win_df has to be "min" and the 2nd column has to be "max"')
  }
  for (i in 1:length(win_df$min)) {
    if( win_df$min[i] >= win_df$max[i] ){
      stop('The "min" values of win_df have to be less than the "max" values of win_df')
    }
  }

  #####body of the code
  names.df <- names(df)
  names(df) <- c('X', 'Y')

  #subset each of the dataframe within the given window
  L <- vector(mode = 'list', length = length(win_df$min))
  for (i in 1:length(win_df$min)) {
    L[[i]] <- subset(df, X <= win_df$max[i])
    L[[i]] <- subset(L[[i]], X >= win_df$min[i] )
  }
  #row bind the dataframes together
  df_reg <- data.frame( X = NULL, Y = NULL)
  #names(df_reg) <- c('X', 'Y')
  for (i in 1:length(L)) {
    df_reg <- rbind(df_reg, L[[i]])
  }

  #evaluate a linear background through the df_reg dataframe
  reg_vec <- linreg(df_reg)
  lin.bg <- reg_vec[1]*df$X + reg_vec[2]
  #subtract the linear background from the original dataframe
  res_df <- df
  res_df$Y <- res_df$Y - lin.bg
  names(res_df) <- names.df

  #plot the raw data along with the background if pl = T
  if( pl == T){
    plot(df$X, df$Y, col = 'black', type = 'b')
    lines(df$X, lin.bg, col = 'red')
  }

  #return a list containing the linear backround coefficients and the background subtracted dataset
  L_ret <- vector(mode = 'list', length = 2)
  names(L_ret) <- c('bg_coeff', 'sub_data')
  L_ret[[1]] <- reg_vec
  L_ret[[2]] <- res_df
  return(L_ret)
}
