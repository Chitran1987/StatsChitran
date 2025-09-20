#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector Arith_Geom_Prog(const double st, const double d, const double r, const int n){
  NumericVector X(n);
  double* p = &X[0]; double* p_end = p+size_t(n); int ind=1;
  double ap_trm = st; double gp_trm = 1.0;
  while(p < p_end){
    *p = ap_trm*gp_trm; p++;
    ap_trm = ap_trm + d;
    gp_trm = gp_trm*r;
  }
  return X;
}

