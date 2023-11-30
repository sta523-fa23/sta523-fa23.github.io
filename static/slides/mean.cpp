#include <Rcpp.h>

//[[Rcpp::plugins(cpp11)]]

//[[Rcpp::export]]
double cpp_mean(double x, double y) {
  return (x+y)/2;
}

/*** R
x <- runif(1e5)
bench::mark(
  cpp_mean(1, 2),
  mean(c(1, 2))
)
*/

