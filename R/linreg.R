#write a program for calculating the linear regression by setting the variable gradients to zero to
#minimize the sum squared error function
#multicolinearity of input variables is a problem and should be addressed in the help files/help links
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
    res<-solve(m,b)
    return(res)
  }
}
