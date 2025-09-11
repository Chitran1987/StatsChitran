#write a function for rotation
rot.axis <- function(df, pt = c(0, 0), deg){
  df.names <- names(df) #extract the names of the data frame
  names(df) <- c('X', 'Y') #renmae the data frame
  #error checking
  #df has to be a dataframe
  if( is.data.frame(df) == F){
    stop('df has to be a dataframe')
  }
  #df has to have two columns
  if( dim(df)[2] != 2){
    stop('df has to be a 2 column dataframe')
  }
  #pt has to be a numeric vector
  if( is.numeric(pt) == F ){
    stop('pt has to be a numeic 2 element vector')
  }
  #pt has to have two elements
  if( length(pt) != 2 ){
    stop('pt has to be a numeric 2 element vector')
  }
  #deg has to be a single element numeric
  if( is.numeric(deg) == F ){
    stop('deg has to be a numeric scalar')
  }
  #deg has to have a length of one
  if( length(deg) !=  1 ){
    stop('deg has to be a numeric scalar')
  }


  #####body of code
  #covert degress to radians
  ang <- deg2rad(deg, overlap = T)
  #rotation matrix
   M <- matrix(data = c( cos(ang), -sin(ang), sin(ang), cos(ang)), ncol = 2, byrow = T )
  #translation step 1
   df1 <- df
   df1$X <- df1$X - pt[1]
   df1$Y <- df1$Y - pt[2]
  #rotation after translation
   R1_X <- NULL
   R1_Y <- NULL
   for (i in 1:length(df1$X)) {
     dmp <- M %*% matrix( data = as.vector(df1[i,], mode = 'numeric'), ncol = 1, byrow = T )
     dmp <- c(dmp[1, 1], dmp[2, 1] )
     R1_X <- c(R1_X, dmp[1])
     R1_Y <- c(R1_Y, dmp[2])
     R1 <- data.frame(R1_X, R1_Y)
   }
   names(R1) <- c('X', 'Y')
   #reshifting after rotation
   R <- R1
   R$X <- R$X + pt[1]
   R$Y <- R$Y + pt[2]
  names(R) <- df.names
  return(R)
}




