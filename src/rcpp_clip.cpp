#include <Rcpp.h>
using namespace Rcpp;

//' Clip vector between numbers a and b
//'
//' @param x Input vector
//' @param a Minumum value
//' @param b Maximum value
//' @export
// [[Rcpp::export]]
NumericVector rcpp_clip( NumericVector x, double a, double b) {
  return clamp( a, x, b );
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
rcpp_clip(-100, 0, 100)
*/
