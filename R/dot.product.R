#Write a function to calculate the dot product between two vectors
dot.product <- function(X, Y){
  ###error checking
  if(!is.numeric(X) || !is.numeric(Y)){
    stop('X and Y should be numeric')
  }
  if(length(X) != length(Y)){
    stop('X and Y need to have the same length')
  }
  if(any(is.na(X)) || any(is.na(Y))){
    stop('check for NAs in X and Y')
  }
  if(length(X) == 0){
    return(0)
  }
  return(dot_prd(X, Y))
}
