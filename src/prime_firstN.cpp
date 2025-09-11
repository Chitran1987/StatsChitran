#include <Rcpp.h>
#include "prime_helpers.h"
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

// Write a code to find the first n prime nos


// [[Rcpp::export]]
NumericVector prime_firstN(int n) {
  NumericVector res;
  int it = 2; //The iteration variable
  while(res.size() < n){
    if(primefunc(it)){
      res.push_back(it);
    }
    it++;
  }
  return(res);

}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//




