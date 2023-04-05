#minimize the residual sum of squares
#multicolinearity of input variables is a problem and should be addressed in the help files/help links


#' function for calculating the linear regression
#'
#' @param df input a dataframe with more than two columns. The last column is always presumed to be the
#' column of the independent variables in the perictor equation while the the others are presumed to be
#' the dependent variables.
#'
#' @return A dataframe comprising of your the estimated values of the regression weights if the input
#' data does not suffer from multicollinearity
#' @export
#'
#' @examples
#' linreg(reg_data1)
#' [,1]
#' [1,] 2.950339
#' [2,] 5.052787
#' [3,] 0.999999
#' [4,] 9.599983
#' (The reg_data1 dataset is available with the "StatsChitran" package)
#'
#' linreg(reg_data2)
#' NULL
#' (The reg_data2 dataset is available with "StatsChitran" package. The function returns NULL in case
#' is an issue with multicollinearity in the input dataset)
linreg<-function(df){
  m<-matrix(NA, ncol=dim(df)[2], nrow = dim(df)[2])
  for (i in 1:(dim(m)[1])) {
    for (j in 1:(dim(m)[1])) {
      if(i!=dim(m)[1] & j!=dim(m)[2]){
        m[i,j]<-sum((df[,i])*(df[,j]))
      }
      else{
        if(i==dim(m)[1] & j==dim(m)[1]){
          m[i,j]<-dim(df)[1]
        }
        else{
          if(i==dim(m)[1]){
            m[i,j]<-sum(df[,j])
          }
          else{
            m[i,j]<-sum(df[,i])
          }
        }
      }

    }

  }
  #creating the vector
  v<-vector(mode = 'numeric', length =dim(m)[1] )
  for (i in 1:length(v)) {
    if(i!=length(v)){
      v[i]<-sum((df[,i])*(df[,length(v)]))
    }
    else{
      v[i]<-sum(df[,i])
    }
  }
  b<-matrix(v, ncol = 1)
  if(det(m)<10^{-15}){
    return(NULL)
    print('matrix computationally singular')
  }
  else{
    res<-as.vector(solve(m,b))
    return(res)
  }
}
