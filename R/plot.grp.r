plot.grp <- function(L, pl.typ = 'l', lin.wid = 1, norm.gr = T, pl.col = 'black', x.lim, y.lim){
  ###error control
  ##L has to be a list
  if(!is.list(L)){
    stop('L has to be a list')
  }
  #check if everything in L is a dataframe
  if( !all( vapply( L, function(x) is.data.frame(x) && ncol(x) == 2, logical(1) ) ) ){
    stop('Every element of L has to be a two column dataframe')
  }
  #norm has to be a boolean bit
  if(!is.logical(norm.gr) || length(norm.gr)!=1 ){
    stop('norm has to be a boolean bit')
  }
  #pl.typ has to be a character
  if(!is.character(pl.typ) || length(pl.typ)!=1 ){
    stop('pl.typ has to be a single character')
  }
  #x.lim and y.lim have to be actual 2 element numeric vectors
  if(!missing(x.lim)){
    if(!is.numeric(x.lim) || length(x.lim)!=2){
      stop('x.lim has to be a two element numeric vector')
    }
  }
  if(!missing(y.lim)){
    if(!is.numeric(y.lim) || length(y.lim)!=2){
      stop('y.lim has to be a two element numeric vector')
    }
  }

  ####actual code
  #normalize check
  N <- length(L)
  if(norm.gr){
    for (i in 1:N) {
      df <- L[[i]]
      names(df) <- c('X', 'Y')
      if(!missing(x.lim)){
        df <- df[df$X >= x.lim[1] & df$X <= x.lim[2],]
      }
      if(!missing(y.lim)){
        df <- df[df$Y >= y.lim[1] & df$Y <= y.lim[2],]
      }
      df$Y <- nrm(df$Y)
      L[[i]] <- df
    }
  }
  ##graph limits
  #set x.lim
  if(missing(x.lim)){
    x_dmp <- NULL
    for(i in 1:N){
      df <- L[[i]]
      names(df) <- c('X', 'Y')
      x_dmp <- c(x_dmp, c(min(df$X), max(df$X)))
    }
    x.lim <- c(min(x_dmp), max(x_dmp))
  }
  #set y.lim
  if(missing(y.lim)){
    y_dmp <- NULL
    for(i in 1:N){
      df <- L[[i]]
      names(df) <- c('X', 'Y')
      y_dmp <- c(y_dmp, c(min(df$Y), max(df$Y)))
    }
    y.lim <- c(min(y_dmp), max(y_dmp))
  }
  ###plotting
  for (i in 1:N) {
    df <- L[[i]]
    names(df) <- c('X', 'Y')
    if(i == 1){
      plot(df$X, df$Y, type=pl.typ, lwd=lin.wid, col=pl.col, xlim = x.lim, ylim = y.lim)
    }else{
      points(df$X, df$Y, type=pl.typ, lwd=lin.wid, col=pl.col)
    }
  }

}
