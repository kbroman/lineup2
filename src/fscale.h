// standardize the columns of a matrix
#ifndef FSCALE_H
#define FSCALE_H

#include <Rcpp.h>

// fscale: standardize columns of a matrix
//  The one-pass method can have a lot of round-off error, but it is quick.
Rcpp::NumericMatrix fscale(const Rcpp::NumericMatrix& x);

// fscalev: standardize a single vector
// The one-pass method can have a lot of round-off error, but it is quick.
Rcpp::NumericVector fscalev(const Rcpp::NumericVector& x);

// fscalev_noNA: standardize a single vector where both of two vectors are not missing
Rcpp::NumericVector fscalev_noNA(const Rcpp::NumericVector& x,
                                 const Rcpp::NumericVector& y);

#endif // FSCALE_H
