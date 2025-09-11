#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include "calculus_helpers.h"
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
double num_integrate_cpp(NumericVector X, NumericVector Y, double xmin, double xmax) {
  double sum = 0.0;
  auto xmin_it = std::lower_bound(X.begin(), X.end(), xmin); //return the iterator to the first element >= xmin
  auto xmax_it = std::upper_bound(X.begin(), X.end(), xmax); //return the iterator to the first element > xmax
  size_t i_min = std::distance(X.begin(), xmin_it); //calculates how far apart iterator xmin_it is from X.begin()
  size_t i_max = std::distance(X.begin(), xmax_it); //calculates how far apart iterator xmax_it is from X.begin()

  for (size_t i = i_min; i < i_max - 1; i++)
  {
    sum = sum + (X[i + 1] - X[i]) * (Y[i] + Y[i + 1]) / 2;
  }

  return sum;
}

//Riemann summation
// [[Rcpp::export]]
double RSum(NumericVector X, NumericVector Y, double xmin, double xmax){
  double res = 0;
  auto xmin_it = std::lower_bound(X.begin(), X.end(), xmin);
  auto xmax_it = std::upper_bound(X.begin(), X.end(), xmax);
  size_t i_min = std::distance(X.begin(), xmin_it);
  size_t i_max = std::distance(X.begin(), xmax_it);
  for (size_t i = i_min; i < i_max - 1; i++)
  {
    res += (X[i + 1] - X[i]) * Y[i];
  }
  return res;
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//


