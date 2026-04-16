#include<iostream>
#include<vector>
#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector table_func(NumericVector X, NumericVector BP, NumericVector H){
  std::size_t N = X.size();
  std::size_t N1 = BP.size();
  std::size_t N2 = H.size();
  NumericVector Y(N);
  for(std::size_t i = 0; i < N1; i++ ){
    if(i == 0){
      for(std::size_t k = 0; k < N; k++){
        if( X[k] <= BP[i]){
          Y[k] = H[0];
        }
      }
    }
    else{
      for(std::size_t k = 0; k < N; k++){
        if(X[k] <= BP[i] && X[k] > BP[i-1]){
          Y[k] = H[i];
        }
      }
    }
    if(i == N1-1){
      for(std::size_t k=0; k < N; k++){
        if(X[k] > BP[N1-1]){
          Y[k] = H[i+1];
        }
      }
    }
  }
  return Y;
}
