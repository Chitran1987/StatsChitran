#Write a code for interpolating curves automatically
interp.curves <- function(L, X, interp = 'spline'){

  #### Error checking

  # L has to be a list
  if(!is.list(L)){
    stop('L has to be a list')
  }

  # L has to be a list of two-column dataframes
  if(!all(vapply(L, function(x) is.data.frame(x) && ncol(x) == 2, logical(1)))){
    stop('L should be a list of two column dataframes')
  }

  #The dataframes need to be numeric
  if(!all(vapply(L, function(x) all(vapply(x, is.numeric, logical(1))), logical(1)))){
    stop('Both columns of each dataframe in L have to be numeric')
  }

  #X has to be a numeric vector
  if(!is.numeric(X)){
    stop('X has to be a numeric vector')
  }

  #interp has to be either 'spline' or 'linear'
  if(!is.character(interp) || length(interp) != 1 || !interp %in% c("spline", "linear")){
    stop('argument "interp" should either be "spline" or "linear"')
  }
  ####core code
  for (i in seq_along(L)) {
    df <- L[[i]]
    names(df) <- c('X', 'Y')
    if(interp == 'spline'){
      dmmy <- spline(x=df$X, y=df$Y, xout=X)
    }else{
      dmmy <- approx(x=df$X, y=df$Y, xout=X)
    }
    df <- data.frame(X=dmmy$x, Y=dmmy$y)
    L[[i]] <- df
  }
  return(L)

}
