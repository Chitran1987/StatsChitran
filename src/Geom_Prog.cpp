#include <Rcpp.h>
#include <vector>
using namespace Rcpp;



// [[Rcpp::export]]
std::vector<long double> Geom_Prog(long double st, long double r, int n){
  std::vector<long double> X(n);
  long double* p = X.data(); long double* p_end = p + n;
  long double el = st;
  while( p < p_end){
    *p = el; p++; el = el*r;
  }
  return X;
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//
