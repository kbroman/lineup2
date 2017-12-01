#ifndef DIST_BETW_MATRICES_H
#define DIST_BETW_MATRICES_H

#include <Rcpp.h>

// calculate the distance between rows of matrix x and rows of matrix y
// d(i,j) = sqrt{ sum_k [x(i,k) - y(j,k)]^2 }
Rcpp::NumericMatrix dist_betw_matrices(Rcpp::NumericMatrix x,
                                       Rcpp::NumericMatrix y);

#endif // DIST_BETW_MATRICES
