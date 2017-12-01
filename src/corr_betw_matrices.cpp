// correlations between columns of two matrices
#include "fscale.h"
#include "corr_betw_matrices.h"
#include <Rcpp.h>

using namespace Rcpp;

// calculate correlation between columns of x and corresponding columns of y
//
// [[Rcpp::export()]]
NumericVector corr_betw_matrices_paired(const NumericMatrix& x, const NumericMatrix& y)
{
    const int n_row = x.rows();
    const int n_col = x.cols();
    if(n_row != y.rows() || n_col != y.cols())
        throw std::invalid_argument("dim(x) != dim(y)");

    // scale the columns of each
    // non-finite values -> NA_REAL
    NumericMatrix xs = fscale(x);
    NumericMatrix ys = fscale(y);

    NumericVector result(n_col);


    for(int j=0; j<n_col; j++) {
        double sum=0.0;
        int count=0;
        for(int i=0; i<n_row; i++) {
            if(isfinite(x(i,j)) && isfinite(y(i,j))) {
                sum += (xs(i,j) * ys(i,j));
                count++;
            }
        }
        if(count > 1) result[j] = sum/(double)(count-1);
        else result[j] = NA_REAL;
    }

    return result;
}

// for each column of left matrix, find the column in the right matrix
// with the highest correlation
//
// [[Rcpp::export()]]
List corbetw2mat_unpaired_bestright(const NumericMatrix& x,
                                             const NumericMatrix& y)
{
    const int n_row = x.rows();
    if(y.rows() != n_row)
        throw std::invalid_argument("nrow(x) != nrow(y)");
    const int n_col_x = x.cols();
    const int n_col_y = y.cols();

    // to contain the results
    NumericVector corr(n_col_x);
    IntegerVector yindex(n_col_x);

    for(int xcol=0; xcol < n_col_x; xcol++) {
        NumericVector xs = fscalev(x(_,xcol));

        double best_corr = -2;
        int best_index = NA_INTEGER;

        for(int ycol=0; ycol < n_col_y; ycol++) {
            NumericVector ys = fscalev(y(_,ycol)); // doing repeatedly but saving memory

            double sum=0.0;
            int count=0;
            for(int i=0; i<n_row; i++) {
                if(isfinite(xs[i]) && isfinite(ys[i])) {
                    sum += (xs[i] + ys[i]);
                    count++;
                }
            }
            if(count > 1) {
                sum /= (double)(count-1);
                if(sum > best_corr) {
                    best_corr = sum;
                    best_index = ycol+1;
                }
            }

        } /* end loop over col of y */
        if(best_corr < -1.0) {
            corr[xcol] = NA_REAL;
            yindex[xcol] = NA_INTEGER;
        }
        else {
            corr[xcol] = best_corr;
            yindex[xcol] = best_index;
        }

    } /* end loop over col of x */

    // combine the results into a list
    return List::create(Named("corr")=corr,
                        Named("yindex")=yindex);

}




// return correlations between column of left matrix and column of right matrix
// that exceed corr_threshold
//
// [[Rcpp::export()]]
List corbetw2mat_unpaired_best(const NumericMatrix& x,
                               const NumericMatrix& y,
                               const double corr_threshold)
{
    const int n_row = x.rows();
    if(y.rows() != n_row)
        throw std::invalid_argument("nrow(x) != nrow(y)");
    const int n_col_x = x.cols();
    const int n_col_y = y.cols();

    // to contain the results
    std::vector<double> corr;
    std::vector<int> xindex;
    std::vector<int> yindex;

    for(int xcol=0; xcol < n_col_x; xcol++) {
        NumericVector xs = fscalev(x(_,xcol));
        for(int ycol=0; ycol < n_col_y; ycol++) {
            NumericVector ys = fscalev(y(_,ycol)); // doing repeatedly but saving memory

            double sum = 0.0;
            int count = 0;

            for(int i=0; i<n_row; i++) {
                if(isfinite(xs[i]) && isfinite(ys[i])) {
                    sum += (xs[i] * ys[i]);
                    count++;
                }
            }
            if(count > 1) {
                sum /= (double)(count-1);
                if(sum >= corr_threshold) {
                    corr.push_back(sum);
                    xindex.push_back(xcol+1);
                    yindex.push_back(ycol+1);
                }
            }

        } /* end loop over col of y */
    } /* end loop over col of x */

    // combine the results into a list
    return List::create(Named("corr")=corr,
                        Named("xindex")=xindex,
                        Named("yindex")=yindex);
}

// calculate full set of correlations between columns of x and columns of y
//
// [[Rcpp::export()]]
NumericMatrix corbetw2mat_unpaired_all(const NumericMatrix& x,
                                       const NumericMatrix& y)
{
    const int n_row = x.rows();
    if(y.rows() != n_row)
        throw std::invalid_argument("nrow(x) != nrow(y)");
    const int n_col_x = x.cols();
    const int n_col_y = y.cols();

    NumericMatrix result(n_col_x, n_col_y);

    for(int ycol=0; ycol<n_col_y; ycol++) {
        NumericVector ys = fscalev(y(_,ycol));

        for(int xcol=0; xcol<n_col_x; xcol++) {

            double sum=0.0;
            int count = 0;

            // doing this repeatedly to save memory
            // (assuming memory more important than time)
            NumericVector xs = fscalev(x(_,ycol));

            for(int i=0; i<n_row; i++) {
                if(isfinite(xs[i]) && isfinite(ys[i])) {
                    sum += (xs[i] * ys[i]);
                    count++;
                }
            }

            if(count > 1) result(xcol, ycol) = sum/(double)(count-1);
            else result(xcol, ycol) = NA_REAL;

        } /* end loop over col of y */
    } /* end loop over col of x */

    return result;
}
