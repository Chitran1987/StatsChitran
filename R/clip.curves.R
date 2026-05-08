#Create a function for clipping a list of dataframes
clip.curves <- function(L, bounds, two.sided = c(TRUE, TRUE)){

  ### Variable declaration
  N <- length(L)

  #### Error checking

  # L has to be a list
  if(!is.list(L)){
    stop('L has to be a list')
  }

  # L has to be a list of two-column dataframes
  if(!all(vapply(L, function(x) is.data.frame(x) && ncol(x) == 2, logical(1)))){
    stop('L should be a list of two column dataframes')
  }

  # two.sided has to be a logical vector
  if(!is.logical(two.sided) || length(two.sided) != 2){
    stop('two.sided has to be a two element logical vector')
  }

  # if two.sided elements are all FALSE, then return L
  if(!any(two.sided)){
    return(L)
  }

  # bounds has to be a two element numeric vector
  if(!is.numeric(bounds) || length(bounds) != 2){
    stop('bounds has to be a two element numeric vector')
  }

  # bounds inner compatibility
  if(bounds[1] >= bounds[2]){
    stop('bounds[1] should be less than bounds[2]')
  }

  # sorting of listed dataframes
  for(i in seq_len(N)){
    df <- L[[i]]
    names(df) <- c('X', 'Y')

    if(is.unsorted(df$X)){
      stop('Dataframe within list L is not sorted')
    }
  }

  # bounds exterior compatibility
  for(i in seq_len(N)){
    df <- L[[i]]
    names(df) <- c('X', 'Y')

    if(two.sided[1] && bounds[1] >= max(df$X)){
      stop('lower bound not within data range')
    }

    if(two.sided[2] && bounds[2] <= min(df$X)){
      stop('upper bound not within data range')
    }
  }

  ### Core code
  for(i in seq_len(N)){
    df <- L[[i]]
    names(df) <- c('X', 'Y')

    if(all(two.sided)){
      df <- df[df$X >= bounds[1] & df$X <= bounds[2], ]
    } else {
      if(two.sided[1]){
        df <- df[df$X >= bounds[1], ]
      } else {
        df <- df[df$X <= bounds[2], ]
      }
    }

    L[[i]] <- df
  }

  return(L)
}
