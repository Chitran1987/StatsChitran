// [[Rcpp::depends(RcppEigen)]]
//[[Rcpp::plugins(cpp17)]]
#include<RcppEigen.h>
#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <iostream>
//#include <Eigen/Dense>
#include <stdexcept>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector kernel_movavgf(NumericVector Y, int bn, int fn, NumericVector w, int ord) {
  //Core program
  int N = Y.size();
  NumericVector res(Y.size()); //vector to store the result
  Eigen::Map<Eigen::VectorXd> we(&w[0], static_cast<Eigen::Index>(w.size()));
  for(int k = 0; k < ord; ++k){
    Eigen::Map<Eigen::VectorXd> Ye(&Y[0], static_cast<Eigen::Index>(Y.size()));
    for (int i = 0; i < Y.size(); ++i) {
      int Ye_st =  std::max(0, i-bn); int Ye_end = std::min(i+fn, N-1); int Ye_len = Ye_end - Ye_st + 1;
      int w_st =  std::max(bn-i, 0); int w_end = std::min(bn+fn, N-2-i+fn); int w_len = w_end - w_st + 1;
      res[i] = Ye.segment(Ye_st, Ye_len).dot(we.segment(w_st, w_len ));
      res[i] = res[i]/we.segment(w_st, w_len).sum();
    }
    Y = res;
  }
  return res;
}
