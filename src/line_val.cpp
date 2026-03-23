#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector line_val(double pX, double pY, double qX, double qY, NumericVector X) {
  double m = (pY - qY) / (pX - qX);
  double c = pY - m * pX;

  NumericVector::iterator ptr = X.begin();
  NumericVector::iterator ptr_end = X.end();

  while (ptr < ptr_end) {
    *ptr = m * (*ptr) + c;
    ++ptr;
  }

  return X;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

