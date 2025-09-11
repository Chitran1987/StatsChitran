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


//define the workers
static void worker(NumericVector X, NumericVector Y, double xmin, double xmax, double& res_dmp ) {
  res_dmp = num_integrate_cpp(X, Y, xmin, xmax);
}


//define the parallel loop
// [[Rcpp::export]]
double num_integrate_par(NumericVector X, NumericVector Y, double xmin, double xmax) {
  //No of threads to use
  double nth = std::thread::hardware_concurrency() - 1;
  if (nth <= 0) return num_integrate_cpp(X, Y, xmin, xmax);

  //divide data
  vector<vector<double>> dat(nth);
  double del = ((xmax - xmin) / nth);
  for (size_t i = 0; i < size_t(nth - 1.0); i++)
  {
    dat[i] = { xmin + i*del, xmin + (i + 1)*del };
  }
  dat[nth-1] = { xmin + (nth - 1) * del, xmax };

  //Place the data chunks into threads
  vector<thread>t(nth);

  //build the data holding vector
  vector<double> res(nth);

  //run the threads
  for (size_t i = 0; i < nth; i++)
  {
    t[i] = thread(worker, X, Y, dat[i][0], dat[i][1], ref(res[i]));
  }

  //join the threads
  for (size_t i = 0; i < nth; i++)
  {
    t[i].join();
  }

  //concatenate the results
  double sum = 0.0;
  for (size_t i = 0; i < nth; i++)
  {
    sum += res[i];
  }
  return sum;

}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//


