#ifndef whichWithin_H
#define whichWithin_H

#include <Rcpp.h>
using namespace Rcpp;
#ifdef _OPENMP
#include <omp.h>
#endif

void aminmax_dbl(double ans[4], DoubleVector x, DoubleVector y, int nThread);


#endif
