#include <Rcpp.h>
#include "prime_helpers.h"
using namespace Rcpp;


//The boolean primefunc
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



//The integer primefunc_int
int primefunc_int(int n){

  if( n == 1){
    return(0);
  }
  else if((n == 2) || (n == 3)){
    return(1);
  }
  else{
    for (int i = 2; i*i <= n; i++) {
      if(n % i == 0){
        return(0);
      }
    }
  }
  return(1);
}
