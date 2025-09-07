#include <Rcpp.h>
#include <vector>
#include <thread>
#include <algorithm>
#include "calculus_helpers.h"


using Rcpp::NumericVector;
using Rcpp::IntegerVector;
using Rcpp::LogicalVector;
using std::vector;
using std::thread;
using std::size_t;
using std::copy;
using std::ref;
// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

//The worker function
void worker(NumericVector X, NumericVector Y, double xmin, double xmax, double& sum) {
  sum = RSum( X, Y, xmin, xmax);
}

// [[Rcpp::export]]
double RSum_par(NumericVector X, NumericVector Y, double xmin, double xmax) {
  //get no of cores
  int N = thread::hardware_concurrency() - 1;
  if (N <= 0) N = 1;
  //divide the data for each thread
  vector<vector<double>> dat(N);
  double del = (xmax - xmin) / N;
  for (size_t i = 0; i < N-1; i++)
  {
    dat[i] = {xmin + i*del, xmin + (i + 1)*del};
  }
  dat[N - 1] = { xmin + (N - 1) * del, xmax };
  //Start the threads and join them
  vector<thread>t(N);
  vector<double> sum(N); double res = 0.0;
  for (size_t i = 0; i < N; i++)
  {
    t[i] = thread(worker, X, Y, dat[i][0], dat[i][1], ref(sum[i]));
  }
  for (size_t i = 0; i < N; i++)
  {
    t[i].join();
  }
  for (size_t i = 0; i < N; i++)
  {
    res = res + sum[i];
  }
  return res;

}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//


