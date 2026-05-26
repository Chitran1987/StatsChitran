#Write a function for polynomial regression
polreg <- function(df, degree, pl=T){
  ######Error checking
  #If df is a 2-column dataframe
  if(!is.data.frame(df) || dim(df)[2]!=2){
    stop('Argument df should be a 2-column dataframe')
  }
  #If df is numeric everywhere
  if(!all(vapply(df, is.numeric, TRUE))){
    stop('All columns of argument df should be a numeric')
  }
  #If degree is a positive integer
  if( !is.numeric(degree) || degree%%1 != 0 || degree <= 0){
    stop('The degree of the polynomial should be an integer')
  }
  #If pl is a boolean
  if(!is.logical(pl) || length(pl)!=1){
    stop('pl should be a boolean bit')
  }

  ######Core algorithm for the logic
  org.nam <- names(df) #Store the original column names
  names(df) <- c('X', 'Y') #Change the column names
  X <- df$X
  Y <- df$Y
  #Create the dataframe for the linear regression
  for (n in rev(1:degree)) {
    if(n == degree){
      dmp = X^n
      col.nam <- paste0("X^",n)
      df_new <- data.frame(col.nam=dmp)
    }else{
      dmp=X^n
      col.nam <- paste0("X^",n)
      df_new[col.nam] <- dmp
    }
  }
  df_new["Y"] <- Y
  #Execute the linear regression
  coeff <- linreg(df=df_new)
  coeff1 <- rev(coeff)
  const <- coeff1[1]
  #Create the polynomial data frame for returning
  dmp <- rep(0, length(X))
  for (n in 0:degree) {
    dmp <- dmp + (X^n)*coeff1[n+1]
  }
  df.res <- data.frame(X = X, Y = dmp)
  names(df.res) <- org.nam
  if(pl){
    names(df) <- org.nam
    plot(df[,1], df[,2], xlab=org.nam[1], ylab=org.nam[2])
    lines(df.res[,1], df.res[,2], col='red')
  }
  #create the return list
  ret.list <- vector(mode = 'list', length = 2)
  ret.list[[1]] <- coeff
  ret.list[[2]] <- df.res
  names(ret.list) <- c('coeff', 'fit dataframe')
  return(ret.list)
}
