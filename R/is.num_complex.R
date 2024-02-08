#check to see if the argument is numeric or complex
is.num_complex <- function(X){
  return(is.numeric(X) | is.complex(X))
}

