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


//define the function primefunc
//Returns whether a value is a prime or not
// [[Rcpp::export]]
bool primefunc(int n){

  if( n == 1){
    return(false);
  }
  else if((n == 2) || (n == 3)){
    return(true);
  }
  else{
    for (int i = 2; i*i <= n; i++) {
      if(n % i == 0){
        return(false);
      }
    }
  }
  return(true);
}
//func defn: primefunc finished


// [[Rcpp::export]]
LogicalVector is_prime(NumericVector x) {


  //Loop through x using
  LogicalVector dmp(x.size());
  for (int i = 0; i <= x.size()-1; i++) {
    dmp.at(i) = primefunc(x.at(i));
  }

  return(dmp);
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//


