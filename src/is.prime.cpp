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


