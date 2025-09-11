#include <Rcpp.h>
#include <vector>
#include <numeric>
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
NumericVector movavg_cpp(NumericVector Y, size_t bn, size_t fn, size_t ord) {
  size_t n = Y.size();
  NumericVector res(n);
  for (size_t j = 1; j <= ord; ++j){
    size_t L = 0, R = std::min(n, fn + 1); //Left and Right sides of the window at i = 0
    double sum = std::accumulate(Y.begin(), Y.begin() + R, 0.0); //Calculate the sum for i = 0
    res[0] = sum / double(R - L);

    for (size_t i = 1; i < n; i++)
    {
      size_t newL = (i > bn) ? (i - bn) : 0;
      size_t newR = std::min(n, i + fn + 1);

      if (newL > L) sum -= Y[L]; //window left edge moved right by 1
      if (newR > R) sum += Y[newR - 1]; //window right edge moved right by 1

      L = newL; R = newR;
      res[i] = sum / double(R - L);

    }
    Y = res;
  }


  return res;
}
// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

