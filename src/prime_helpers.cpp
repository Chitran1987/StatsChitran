#include <Rcpp.h>
#include "prime_helpers.h"
using namespace Rcpp;

bool primefunc(int n){

  if( n == 1){
    return(false);
  }
  else if((n == 2) || (n == 3)){
    return(true);
  }
  else{
    for (int i = 2; i*i <= n; i++) {
      if(n % i == 0){
        return(false);
      }
    }
  }
  return(true);
}
//func defn: primefunc finished
