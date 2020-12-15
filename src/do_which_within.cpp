#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List do_which_within(DoubleVector lat, DoubleVector lon, double r) {
  return List::create(Named("x") = 0);
}

