#include <Rcpp.h>
#include <thread>
#include <vector>
#include "prime_helpers.h"

//Selective imports
using Rcpp::NumericVector;
using Rcpp::IntegerVector;
using Rcpp::LogicalVector;
using std::vector;
using std::thread;
using std::size_t;
using std::copy;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

//Workers for Threading
//declare the worker function
static void worker(int st, int end, vector<int>& c) {
  c.resize(end - st + 1);
  for (int i = st; i <= end; i++)
  {
    int dmp = i - st;
    c.at(dmp) = primefunc_int(i);
  }
}




//declare the function that contains the loopings

// [[Rcpp::export]]

IntegerVector is_prime_par(int N) {
  //declare variables needed
  int nth;
  nth = std::thread::hardware_concurrency() - 1;

  if(nth == 0){
    nth = 1;
  }

  //create datachunks for worker input at each loop
  int del = ceil(N / nth) - 1;
  vector<vector<int>> df(nth);
  vector<int> X(2);
  for (int i = 0; i < nth; i++)
  {
    if (i != nth - 1) {
      df[i] = { 1 + i * del, 1 + (i + 1) * del - 1 };
    }
    else {
      df[i] = { 1 + i * del, N };
    }



  }

  //cout << "status flag zero" << endl;

  //create output data holders
  vector<vector<int>> res_df(nth);

  //create the threads DYNAMICALLY
  vector<thread> t(nth); //thread vector size is same as nth


  //Tell the loops to start pushing to the workers
  for (int i = 0; i < nth; i++)
  {
    t[i] = thread(worker, df[i][0], df[i][1], ref(res_df[i]));
  }


  //cout << "status flag 0" << endl;
  //join the loops
  for (int i = 0; i < nth; i++)
  {
    t[i].join();

  }

  //cout << "status flag 1" << endl;

  //concatenate the vectors
  size_t sz = 0;
  for (size_t i = 0; i < nth; i++)
  {
    sz = sz + res_df[i].size();
  }
  vector<int> res_vec(sz);
  int out = 0;
  for (size_t i = 0; i < nth; i++)
  {
    std::copy(res_df[i].begin(), res_df[i].end(), res_vec.begin() + out);
    out = out + res_df[i].size();
  }
  //return(res_vec);
  Rcpp::IntegerVector res = Rcpp::wrap(res_vec);
  return res;
}



// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

