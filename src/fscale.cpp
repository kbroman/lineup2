// standardize the columsn of a matrix

#include "fscale.h"
#include <Rcpp.h>
using namespace Rcpp;

// fscale: standardize columns of a matrix
//  The one-pass method can have a lot of round-off error, but it is quick.
// [[Rcpp::export()]]
NumericMatrix fscale(const NumericMatrix& x)
{
    const int n_row = x.rows();
    const int n_col = x.cols();

    // result matrix, filled with missing values
    NumericMatrix result(n_row, n_col);

    for(int j=0; j<n_col; j++) {

        double sum=0.0, sumsq=0.0, diff, first=NA_REAL;
        int count=0;

        // get mean and sd
        for(int i=0; i<n_row; i++) {
            if(isfinite(x(i,j))) {
                count++;
                if(!isfinite(first)) first = x(i,j); // first non-missing value
                else {
                    // sum(x) and sum(x*x) with x centered at first non-missing value
                    sum += (diff=(x(i,j)-first));
                    sumsq += (diff*diff);
                }
                result(i,j) = x(i,j);
            }
            else result(i,j) = NA_REAL; // non-finite values -> NA


        }

        // center and scale the column
        if(count > 1) { /* if count < 2, do nothing */
            sumsq = sqrt((sumsq - (sum*sum)/(double)count)/(double)(count-1));
            sum /= (double)count;
            for(int i=0; i<n_row; i++) {
                if(isfinite(x(i,j))) result(i,j) = (x(i,j) - sum - first)/(sumsq);
            }
        }
    }

    return result;
}

// fscalev: standardize a single vector
// The one-pass method can have a lot of round-off error, but it is quick.
// [[Rcpp::export()]]
NumericVector fscalev(const NumericVector& x)
{
    const int n = x.size();

    // result matrix, filled with missing values
    NumericVector result(n);

    double sum=0.0, sumsq=0.0, diff, first=NA_REAL;
    int count=0;

    // get mean and sd
    for(int i=0; i<n; i++) {
        if(isfinite(x[i])) {
            count++;
            if(!isfinite(first)) first = x[i]; // first non-missing value
            else {
                // sum(x) and sum(x*x) with x centered at first non-missing value
                sum += (diff=(x[i]-first));
                sumsq += (diff*diff);
            }
            result[i] = x[i];
        }
        else result[i] = NA_REAL; // non-finite values -> NA
    }

    // center and scale the column
    if(count > 1) { /* if count < 2, do nothing */
        sumsq = sqrt((sumsq - (sum*sum)/(double)count)/(double)(count-1));
        sum /= (double)count;
        for(int i=0; i<n; i++) {
            if(isfinite(x[i])) result[i] = (x[i] - sum - first)/(sumsq);
        }
    }

    return result;
}
