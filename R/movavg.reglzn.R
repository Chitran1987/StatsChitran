####write a moving average function wih objective fitting which includes a regularization penalty
####This is called regularized uniform kernel regression
####The kernel in question is a uniform kernel
movavg.reglzn <- function(X, Y, bn, fn, lambda, ord.min = 1, ord.max = 40, pl = T, grid.search = T){

  ##Library Import
  library(StatsChitran)
  library(rgenoud)
  ##Error check
  if(!is.numeric(lambda)){
    stop('Argument lambda needs to be of type numeric')
  }
  if(!is.logical(pl)){
    stop('Argument pl needs to be a boolean bit')
  }
  if(length(pl) != 1){
    stop('Argument pl needs to be a boolean bit')
  }
  if( (ord.min%%1 != 0) | (ord.max%%1 != 0) ){
    stop('Arguments ord.min and ord.max need to be integers')
  }
  if( (ord.min <= 0) | (ord.max <= 0) ){
    stop('Arguments ord.min and ord.max need to be positive integers')
  }
  if(ord.min > ord.max){
    stop('Argument ord.min needs to be less than or equal to ord.max')
  }
  if(!is.logical(grid.search)){
    stop('Argument grid.search should be a boolean bit')
  }
  if(length(grid.search) != 1){
    stop('Argument grid.search should be a boolean bit')
  }

  ##The order function which returns the objective function (SSE + regularazation term)
  obj.fun <- function(ord){
    ord <- round(ord)
    df <- StatsChitran::movavg(X, Y, bn, fn, ord)
    SSE <- sum((df$Y - Y)^2) #The sum squared error term
    MSE <- SSE/length(df$Y) #The Mean squared error term
    d2Y <- num_diff(df$X, df$Y, pl = F, order = 2)
    d2Y$Y <- d2Y$Y^2
    integration <- num_integrate(d2Y$X, d2Y$Y, xmin = min(d2Y$X), xmax = max(d2Y$X))
    pen <- lambda*integration
    res <- MSE + pen
    return(res)
  }
  if(!grid.search){
    L <- genoud(fn = obj.fun, nvars = 1, Domains = cbind(ord.min, ord.max), data.type.int = T, max = F)
    df <- StatsChitran::movavg(X, Y, bn, fn, ord = round(L$par))
    L_ret <- vector(mode = 'list', length = 3)
    L_ret[[1]] <- L
    L_ret[[2]] <- df
    L_ret[[3]] <- L$par
    names(L_ret) <- c('optim-data', 'smoothed-dataframe', 'par')
    if(pl){
      plot(X, Y)
      lines(df$X, df$Y, col = 'red')
    }
    return(L_ret)
  }else{
    ord.val <- seq(ord.min, ord.max) #sequence of values to be sequenced
    ord.reg <- NULL #sequence of returned MSE+penalty regularization terms
    for (k in ord.val) {
      res <- obj.fun(k)
      ord.reg <- c(ord.reg, res)
    }
    df_grid <- data.frame(ord.val, ord.reg)
    if(pl){
      df <- movavg( X, Y, bn, fn, ord = df_grid$ord.val[df_grid$ord.reg == min(df_grid$ord.reg)] )
      plot(X, Y)
      lines(df$X, df$Y, col = 'red')
    }
    L_ret <- vector(mode = 'list', length = 3)
    L_ret[[1]] <- df_grid
    L_ret[[2]] <- df
    L_ret[[3]] <- df_grid$ord.val[df_grid$ord.reg == min(df_grid$ord.reg)]
    names(L_ret) <- c('GridOfVariance', 'smoothed-dataframe', 'par')
    return(L_ret)
  }

}
