library(StatsChitran)
###write a gaussian average function wih objective fitting which includes a regularization penalty
####This is called regularized gaussian kernel regression
####The kernel in question is a gaussian kernel
gauss.avg.reglzn <- function(X, Y, bn, fn, sig, lambda, ord.min = 1, ord.max, pl = T, grid.search = T){

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
    df <- StatsChitran::gauss.avg(X, Y, bn, fn, sig, ord, pl = F)
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
    df <- StatsChitran::gauss.avg(X, Y, bn, fn, sig, ord = round(L$par))
    L_ret <- vector(mode = 'list', length = 3)
    L_ret[[1]] <- L
    L_ret[[2]] <- df
    L_ret[[3]] <- L$par
    names(L_ret) <- c('optim_data', 'smoothed_dataframe', 'par')
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
      df <- gauss.avg( X, Y, bn, fn, sig, ord = df_grid$ord.val[df_grid$ord.reg == min(df_grid$ord.reg)] )
      plot(X, Y)
      lines(df$X, df$Y, col = 'red')
    }
    L_ret <- vector(mode = 'list', length = 3)
    L_ret[[1]] <- df_grid
    L_ret[[2]] <- df
    L_ret[[3]] <- df_grid$ord.val[df_grid$ord.reg == min(df_grid$ord.reg)]
    names(L_ret) <- c('GridOfVariance', 'smoothed_dataframe', 'par')
    return(L_ret)
  }

}







########Testing
df <- noisy.gaussians
plot(df$X, df$Y)
df1 <- gauss.avg(df$X, df$Y, bn = 3, fn = 3, sig = 1, pl = F, ord = 1)
df2 <- gauss.avg(df$X, df$Y, bn = 3, fn = 3, sig = 1, pl = F, ord = 300)
lines(df1$X, df1$Y, col = 'green')
lines(df2$X, df2$Y, col = 'red')

df_reg <- gauss.avg.reglzn(df$X, df$Y, bn = 3, fn = 3, sig = 1, lambda = 0.005, ord.min = 30, ord.max = 50)
L <- df_reg
L$par
df_reg <- L$GridOfVariance
plot(df_reg$ord.val, df_reg$ord.reg)

plot(df$X, df$Y)
df1 <- gauss.avg(df$X, df$Y, bn = 3, fn = 3, sig = 1, pl = F, ord = 1)
df2 <- gauss.avg(df$X, df$Y, bn = 3, fn = 3, sig = 1, pl = F, ord = 35)
lines(df1$X, df1$Y, col = 'green')
lines(df2$X, df2$Y, col = 'red')
