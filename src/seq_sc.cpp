#include <Rcpp.h>
#include<vector>
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
std::vector<double> seq_sc(double st, double end, int n){
  std::vector<double> X(n); double* ps = &X[0]; double* pe = &X[n-1]; double* pinc = ps;
  double dmp = st; double del = (end - st)/(n - 1);
  while(pinc <= pe){
    *pinc = dmp;
    dmp = dmp+del;
    pinc++;
  }

  return X;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//


