// [[Rcpp::depends(RcppEigen)]]
//[[Rcpp::plugins(cpp17)]]
#include<RcppEigen.h>
#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
double dot_prd(NumericVector X, NumericVector Y) {
  Eigen::Map<Eigen::VectorXd> Xe(&X[0], static_cast<Eigen::Index>(X.size()));
  Eigen::Map<Eigen::VectorXd> Ye(&Y[0], static_cast<Eigen::Index>(Y.size()));
  return Xe.dot(Ye);
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

