#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector line_val(double pX, double pY, double qX, double qY, NumericVector X) {
  if (pX == qX) {
    stop("pX and qX must be different.");
  }

  R_xlen_t n = X.size();
  double m = (pY - qY) / (pX - qX);
  double c = pY - m * pX;

  for (R_xlen_t i = 0; i < n; ++i) {
    X[i] = m * X[i] + c;
  }

  return X;
}
