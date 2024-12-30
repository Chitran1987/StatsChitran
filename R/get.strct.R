#Write a program to return the structure of the object
#The structures to sample from are
#dataframe, matrix, array, vector, list and function
#Returns unknown function if not recognized
get.strct <- function(obj) {
  if (is.data.frame(obj)) {
    return("Data frame")
  } else if (is.matrix(obj)) {
    return("Matrix")
  } else if (is.array(obj)) {
    return("Array")
  } else if (is.vector(obj)) {
    return("Vector")
  } else if (is.list(obj)) {
    return("List")
  } else if (is.function(obj)){
    return('function')
  }else {
    return("Unknown structure")
  }
}
