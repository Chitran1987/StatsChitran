movavg <- function(X,Y, bn, fn, ord = 1, base = 'cpp'){
  if( (length(bn) != 1)|(length(fn) != 1) ){
    stop('Inputs to fn and fb should be numeric and single digit')
  }else if(length(X) != length(Y)){
    stop('length of input vectors X and Y need to be the same')
  }else if( ord <= 0 | ord%%1 != 0 ){
    stop('Parmater ord needs to be a positive integer')
  }else{
    if (base != 'cpp'){
      y_res <- NULL
      for (k in 1:ord) {
        y_res <- NULL
        for (i in 1:length(Y)) {
          y_res <- c( y_res, sum( Y[max(1,i-bn): min(i+fn, length(Y))] )/( min(i+fn, length(Y)) - max(1, i-bn) + 1 ) )
        }
        Y <- y_res

      }
      df <- data.frame(X, Y)
      return(df)
    }else{
      Y <- movavg_cpp(Y, bn, fn, ord)
      df <- data.frame(X, Y)
    }

  }
}
