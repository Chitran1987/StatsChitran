#ifndef CALCULUS_HELPERS_H
#define CALCULUS_HELPERS_H

#include <Rcpp.h>

double num_integrate_cpp(Rcpp::NumericVector X, Rcpp::NumericVector Y, double xmin, double xmax); //just a declaration as a helper function
double RSum(Rcpp::NumericVector X, Rcpp::NumericVector Y, double xmin, double xmax); //helper function declaration
//int primefunc_int(int n);//just a declaration as a helper function
//int primefunc_large_int(uint64_t n); //just a declaration as a helper function

#endif
