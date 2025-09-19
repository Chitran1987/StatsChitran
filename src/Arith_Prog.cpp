#include <Rcpp.h>
#include <vector>
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
std::vector<double> Arith_Prog(double st, int n, double d){
  std::vector<double>X(n);
  double* p = X.data(); double* p_End = p + n; double el = st;
  while(p < p_End){
    *p = el; el = el+d; p++;
  }
  return X;
}



// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

