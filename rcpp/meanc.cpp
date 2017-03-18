#include <Rcpp.h>
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
double meanc(NumericVector x){
  int n = x.size();
  int out = 0;
  for(int i = 0; i<n; i++){
    out += x[i];
  }
  return out/n;
}

/***
meanc(1:10)
*/