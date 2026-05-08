#Write a function to calculate group average from a list of dataframes
avg.curves <- function(L, X, pl=T, norm.gr=T, interp = 'spline', lin.wd = 3, pl.col = 'gray'){
  ####Error checking
  #L has to be a list
  if(!is.list(L)){
    stop('L has to be a list of two column data frames')
  }
  #L just can contain two column dataframes
  if(!all( vapply(L, function(x) is.data.frame(x) && ncol(x) == 2, logical(1) ) )){
    stop('Each element of L has to be a two column dataframe')
  }
  #norm.gr has to be a single boolean bit
  if( !is.logical(norm.gr) || length(norm.gr)!=1 ){
    stop('norm.gr has to be a single boolean bit')
  }
  #interp has to be a character scalar
  if( !is.character(interp) || length(interp)!=1){
    stop("'interp' argument has to be either 'spline' or 'linear'")
  }
  #interp has to be either spline or linear
  if( interp!='spline' & interp!='linear'){
    stop("'interp' argument has to be either 'spline' or 'linear'")
  }
  #X has to be a numeric vector
  if( !is.numeric(X)){
    stop('X has to be a numeric vector')
  }
  ####core code
  N <- length(L) #Length of list
  L1 <- L #New list
  ##Interpolate new list
  for (i in seq_along(L)) {
    df <- L[[i]]
    names(df) <- c("X", "Y")

    if (interp == "spline") {
      dmmy <- spline(x = df$X, y = df$Y, xout = X)
    } else {
      dmmy <- approx(x = df$X, y = df$Y, xout = X)
    }

    L1[[i]] <- data.frame(X = dmmy$x, Y = dmmy$y)
  }

  ##Normalize if needed
  if(norm.gr){
    for (i in 1:N) {
      df <- L1[[i]]
      names(df) <- c('X', 'Y')
      df$Y <- nrm(df$Y)
      L1[[i]] <- df
    }
  }

  ##Average now
  Y.vec <- rep(0,length(X))
  for (i in 1:N) {
    df <- L1[[i]]
    Y.vec <- Y.vec + df$Y
  }
  Y.vec <- Y.vec/N #Average the sum

  ##Place reult into a dataframe
  d.fr <- data.frame(X=X, Y=Y.vec) #Place into a dataframe

  if(pl){
    plot.grp(L, lin.wid = lin.wd, norm.gr = norm.gr, pl.col = pl.col)
    lines(d.fr$X, d.fr$Y, col='black')
  }
  if(norm.gr){
    d.fr$Y <- nrm(d.fr$Y)
  }
  return(d.fr)
}
