#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
void line_val(double pX, double pY, double qX, double qY, NumericVector X){
  double m = (pY-qY)/(pX-qX); double c = pY - m*pX;
  double* ptr = X.begin(); double* ptr_end = ptr + size_t(X.size());
  while(ptr < ptr_end){ *ptr = m*(*ptr)+c; ptr++;}
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

