#include "dist_betw_matrices.h"
#include <Rcpp.h>

// calculate the distance between rows of matrix x and rows of matrix y
// d(i,j) = sqrt{ sum_k [x(i,k) - y(j,k)]^2 }
//
// [[Rcpp::export(".dist_betw_matrices")]]
Rcpp::NumericMatrix dist_betw_matrices(Rcpp::NumericMatrix x,
                                       Rcpp::NumericMatrix y)
{
    int n = x.rows();
    int m = y.rows();
    int p = x.cols();
    if(y.cols() != p)
        throw std::invalid_argument("ncol(x) != ncol(y)");

    Rcpp::NumericMatrix result(n,m);

    for(int i=0; i<n; i++) {
        for(int j=0; j<m; j++) {
            Rcpp::checkUserInterrupt();  // check for ^C from user

            double value = 0.0;
            for(int k=0; k<p; k++) value += (x(i,k) - y(j,k))*(x(i,k) - y(j,k));
            result(i,j) = sqrt(value);
        }
    }

    return result;
}
