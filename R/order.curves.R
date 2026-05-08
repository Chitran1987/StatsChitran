#Write a function to sort the dataframes in a list
order.curves <- function(L){
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

  #####Code
  for (i in seq_along(L)) {
    df <- L[[i]]
    names(df) <- c('X', 'Y')
    df <- df[order(df$X),]
    L[[i]] <- df
  }
  return(L)
}
