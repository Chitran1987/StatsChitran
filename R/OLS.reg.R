#Write a program for fitting a model to a dataset using OLS regression
OLS.reg <- function(model, dat, guess, algorithm = 'Nelder-Mead', Delta = 10^-6){

  ######Error checking############################################################
  #model has to be a function
  if(!is.function(model)){
    stop('model has to be a function')
  }
  #dat has to be a dataframe
  if(!is.data.frame(dat)){
    stop('dat is a data frame')
  }
  #is the dataframe dimension consistent with the function parameters
  if(dim(dat)[2] != length(formals(model))){
    stop('Dimensions of dat and model are not compatible')
  }
  #is the dataframe numeric
  if(!all(sapply(df, is.numeric))){
    stop('All columns of dat have to be numeric')
  }
  #is the algorithm input acceptable
  valid.algorithm <- c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN","Brent") #possible choices
  if( !(algorithm %in% valid.algorithm) ){
    stop('algorithm can only be among "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN" or "Brent" ')
  }
  #is Brent acceptable?
  if( algorithm == 'Brent' & (length(guess) != 1) ){
    stop('"Brent" can only be used as an optimization algorithm for univariate parameter space: guess should be a scalar')
  }
  #is Delta numeric
  if(!is.numeric(Delta) || length(Delta) != 1){
    stop("Delta has to be a numeric scalar")
  }
  #does V have the same dimension as guess
  arg_names <- names(formals(model)) #get the name of the arguments of the function
  last_arg <- tail(arg_names, 1)  # Last argument is V
  N <- dim(dat)[2] - 1 #No of X-inputs from data
  err.env <- new.env()#create a new environment which you can later remove
  err.env$L <- as.list(rep(1, times = N)) #creates a dummy list for the X values
  err.env$L[[N+1]] <- guess #Enter the guess vector as the last element in the list
  err.env$bit <- T #bit for error checking in tryCatch()
  err.env$result <-tryCatch({
    err.env$res <- do.call(model, err.env$L)
    if(is.na(err.env$res) || is.nan(err.env$res)){
      err.env$bit = F
    }
    err.env$bit = T
  },
  error = function(e){
    err.env$bit = F
  })
  if(err.env$result == F){
    stop('The dimensions of "guess" donot match the argument inside the model function' )
  }
  rm(err.env) #Remove the error environment after this

  ##########Actual code#################################################################
  ##The cost function
  func.guess <- function(guess){
    ###real code##############################################################
    N <- dim(dat)[2] - 1 #No of X-inputs from data
    pnts <- dim(dat)[1] #No of data points in the regression model
    model.args <- dat[, 1:N, drop = F] #subset the dat dataframe into model.args

    #dynamically allocate values to the function "model"
    arg_names <- names(formals(model)) #get the name of the arguments of the function
    # Identify the last argument name (which corresponds to V)
    last_arg <- tail(arg_names, 1)  # Last argument is V
    # Assign the first n arguments dynamically
    #model.val <- NULL
    model.val <- apply(model.args, 1, function(row) {
      args_list <- as.list(row)
      names(args_list) <- arg_names[1:N]
      args_list[[last_arg]] <- guess
      do.call(model, args_list)
    })


    #create the loss vector
    Loss <- (model.val - dat[,N+1])^2
    Cost <- sum(Loss)/pnts #Mean Squared Error
    return(Cost)
  }

  ##The gradient of the cost function
  grad.guess <- function(guess){
    return(grad.func(f = func.guess, pt = guess, epsilon = Delta))
  }



  #guess optimization
  L <- optim(par = guess, fn = func.guess, gr = grad.guess, method = algorithm)

  #add error to model
  #calculate new model values, model.val_new
  model.args <- dat[, 1:N, drop = F] #subset the dat dataframe into model.args
  model.val_new <- apply(model.args, 1, function(row) {
    args_list <- as.list(row)
    names(args_list) <- arg_names[1:N]
    args_list[[last_arg]] <- L$par
    do.call(model, args_list)
  })
  #calculate error values
  err <- dat[,N+1] - model.val_new
  #add error values to list
  L$err <- err
  #return L
  return(L)
}
