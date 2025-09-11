#Write a program to use rejection sampling to sample from a probability distribution
#Returns a vector of those samples

sample.distr <- function(n, func, f.par, xmin, xmax){
  #n is the no. of needed samples
  #func has to be a function of m-arguments
  #func does not need to be normalized
  #par has to be a vector of m-1 arguments (These m-1 arguments are to be fed to all but the first argument of the function func)
  #xmin and xmax are the values within which the samples will be drawn
  #The sample will be fed to the first argument of the vector

  ########Error Checking######################################################
  # n has to be a positive integer scalar
  if(!is.numeric(n) || (n%%1 != 0) || (length(n) != 1) || (n <= 0)){
    stop("n has to be a positiove scalar integer")
  }
  #func has to be a function
  if(!is.function(func)){
    stop("func accepts arguments that are functions")
  }
  #f.par has to be a vector
  if( !(is.atomic(f.par)) || !(is.vector(f.par)) ){
    stop("f.par has to be an atomic vector")
  }
  #f.par needs to have the dimension of the no. of arguments of func - 1
  err <- new.env()
  err$func.arg <- formals(func)
  err$L.arg <- length(err$func.arg)
  if( (err$L.arg - 1) != length(f.par) ){
    stop('length of f.par has to equal to the one less than the no. of parameters passed to the function func')
  }
  #xmin has to be a numeric scalar
  if(!is.numeric(xmin) || (length(xmin) != 1) ){
    stop("xmin has to be a numeric scalar")
  }
  #xmax has to be a numeric scalar
  if(!is.numeric(xmax) || (length(xmax) != 1) ){
    stop("xmax has to be a numeric scalar")
  }
  #xmin has to be less than xmax
  if(xmin >= xmax){
    stop('Xmin has to be less than xmax')
  }
  ###################### Actual code###################################################################################
  #sampling function definition
  #Returns a sampled dframe which contains the accepted samples and their likelihoods for the given probability distribution for "rej" samples
  samp.fun <- function(rej){
    ####first draw rej*n samples
    X_1 <- runif(rej*n, min = xmin, max = xmax)
    Y_1 <- runif(rej*n, min = 0, max = 1)
    samp.df <- data.frame(X_1, Y_1)
    rm(X_1, Y_1)
    gc()
    names(samp.df) <- c('RV', 'Pu')
    ####define a wrapper function to feed all the non R.V. parameters(f.par) to func and then call the R.V. values like X_1
    wrap <- function(V){
      arg1 <- list(V)
      arg2 <- as.list(f.par)
      par.list <- c(arg1, arg2)
      dummy <- do.call(what = func, args = par.list)
      return(nrm(dummy))
    }
    ####extract the likelihood vectors and add it to the dataframe
    L_hood <- wrap(samp.df$RV)
    samp.df$Lhood <- L_hood
    ####subset the dataframe where Pu < Lhood
    samp.df <- samp.df[samp.df$Pu <= samp.df$Lhood, ]
    return(samp.df)
  }



  #sampling function call
  rejk <- 10
  df <- samp.fun(rejk)


  ####Now check if the no of elements in RV are equal to or greater than n and act accordingly
  ###If greater than or equal to n, sample accordingly
  RV.len <- dim(df)[1]
  if(RV.len >= n){
    dmp1 <- sample(seq(1:RV.len), n, replace = F)
    df <- df[dmp1,]
  }else{
    while(rejk < 1000 && RV.len < n){
      RV.len <- dim(df)[1]
      rejk <- rejk*10
      df <- samp.fun(rejk)
    }
  }
  if(rejk >= 1000 && RV.len < n){
    stop('The probability distribution is too narrow to be rejected by a uniform distribution within this given space. Try using a different distribution.')
  }


  #Check for length of the vector again, since no length check in the while loop
  if(RV.len >= n){
    dmp1 <- sample(seq(1:RV.len), n, replace = F)
    df <- df[dmp1,]
  }


  #create a return list and store the values in them and return
  ret.list <- list()
  ret.list$random_var <- df$RV
  ret.list$likelihood <- df$Lhood
  rm(df)
  gc()
  return(ret.list)
}


