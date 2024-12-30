#Write a program to return the structure of the object
#The structures to sample from are
#dataframe, matrix, array, vector, list, function and unknown
#Returns unknown function if not recognized
get.strct <- function(obj) {
  if (is.data.frame(obj)) {
    return("Data frame")
  } else if (is.array(obj)) {
    if(is.matrix(obj)){
      return("Matrix")
    }else{
      return('Array')
    }
  } else if (is.vector(obj)) {
    if(is.list(obj)){
      return('List')
    }else{
      return('Vector')
    }
  } else if (is.function(obj)){
    return('function')
  }else {
    return("Unknown")
  }
}
