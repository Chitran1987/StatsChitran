#Write a function called grad.func which accepts a function and return the gradient AT A CERTAIN POINT.
grad.func <- function(f, pt, epsilon){
  #f has to be a function which returns a numeric
  if(!is.function(f)){
    stop('Argument f has to be a function')
  }
  #f has to be a function with only one argument
  if(length(formals(f)) != 1){
    stop('The function f needs to have exactly one vector as its input')
  }
  #pt needs to be a numeric vector
  if(!is.vector(pt) || !is.numeric(pt)){
    stop('Argument pt has to be a numeric vector')
  }
  #epsilon needs to be a numeric scalar
  if(!is.numeric(epsilon) || (length(epsilon) != 1)){
    stop('epsilon needs to be a numeric scalar')
  }
  #pt vector needs to have the same size as the arguments of f
  dm_lst <- list()
  dm_lst[[1]] <- pt
  names(dm_lst) <- formals(f)
  dmmy <- tryCatch({ do.call( f, dm_lst )
  },error = function(e){
    message("pt needs to have the same length as the argument to f", e$message)
  })
  if(is.na(dmmy) || is.nan(dmmy)){
    stop("pt needs to have the same length as the argument to f")
  }
  ######Actual Code#########################################################
  n <- length(pt) #No of dimesnions of pt
  res <- NULL #The result vector
  for (i in 1:n) {
    del <- rep(0, times=n)
    del[i] <- epsilon
    dmp <- f(pt + del) - f(pt - del)
    dmp <- dmp /(2*epsilon)
    res <- c(res, dmp)
  }
  return(res)
}
