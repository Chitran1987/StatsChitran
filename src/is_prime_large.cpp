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
//declare the worker function
static void worker(vector<int> X, vector<int>& c) {
  c.resize(X.size());
  for (int i = 0; i < X.size(); i++)
  {
    //int dmp = i - st;
    //c.push_back(is_prime(X.at(i))) ;
    c.at(i) = primefunc_int(X.at(i));
  }
}


//declare the function that initializes the threads
// [[Rcpp::export]]
IntegerVector is_prime_large(IntegerVector X) {
  //declare variables needed
  int nth;
  nth = std::thread::hardware_concurrency() - 1;
  //set nth as at least 1
  if (nth == 0) {
    nth = 1;
  }

  //create datachunks for worker input at each loop
  int N = X.size();
  int del = ceil(N / nth) - 1;
  vector<vector<int>> df(nth);
  //vector<int> X(2);
  for (int i = 0; i < nth; i++)
  {

    if (i == 0) {
      df[i].assign(X.begin(), X.begin() + del);
    }
    else if (i < nth - 1) {
      df[i].assign(X.begin() + i * del, X.begin() + (i + 1) * del);
    }
    else {
      df[i].assign(X.begin() + i * del, X.end());

    }

  }





  //create output data holders
  vector<vector<int>> res_df(nth);

  //create the threads DYNAMICALLY
  vector<thread> t(nth); //thread vector size is same as nth


  //Tell the loops to start pushing to the workers
  for (int i = 0; i < nth; i++)
  {
    //cout << "thread" << i << "started" << endl;
    t[i] = thread(worker, df[i], ref(res_df[i]));
  }



  //join the loops
  for (int i = 0; i < nth; i++)
  {
    t[i].join();


  }


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
  Rcpp::IntegerVector res_vec_R = Rcpp::wrap(res_vec);
  return res_vec_R;

  //return(df);
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

