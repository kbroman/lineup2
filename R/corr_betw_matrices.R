#' Calculate correlations between columns of two matrices
#'
#' For matrices x and y, calculate the correlation between columns of x and
#' columns of y.
#'
#' Missing values (`NA`) are ignored, and we calculate the correlation
#' using all complete pairs, as in [stats::cor()] with
#' `use="pairwise.complete.obs"`.
#'
#' @md
#'
#' @param x A numeric matrix.
#' @param y A numeric matrix with the same number of rows as `x`.
#' @param what Indicates which correlations to calculate and return.  See
#' value, below.
#' @param corr_threshold Threshold on correlations if `what="bestpairs"`.
#' @return If `what="paired"`, the return value is a vector of
#' correlations, between columns of `x` and the corresponding column of
#' `y`.  `x` and `y` must have the same number of columns.
#'
#' If `what="bestright"`, we return a data frame of size `ncol(x)` by
#' `3`, with the \eqn{i}th row being the maximum correlation between
#' column \eqn{i} of `x` and a column of `y`, and then the
#' `y`-column index and `y`-column name with that correlation.  (In
#' case of ties, we give the first one.)
#'
#' If `what="bestpairs"`, we return a data frame with five columns,
#' containing all pairs of columns (with one in `x` and one in `y`)
#' with correlation \eqn{\ge} `corr_threshold`.  Each row corresponds to a
#' column pair, and contains the correlation and then the `x`- and
#' `y`-column indices followed by the `x`- and `y`-column names.
#'
#' If `what="all"`, the output is a matrix of size `ncol(x)` by
#' `ncol(y)`, with all correlations between columns of `x` and
#' columns of `y`.
#'
#' @seealso [dist_betw_matrices()]
#' @export
corr_betw_matrices <-
    function(x, y, what=c("paired", "bestright", "bestpairs", "all"),
             corr_threshold=0.9)
{
    if(!is.matrix(x)) x <- as.matrix(x)
    if(!is.matrix(y)) y <- as.matrix(y)

    if(nrow(x) != nrow(y)) stop("nrow(x) != nrow(y) [", nrow(x), " != ", nrow(y), "]")

    px <- ncol(x)
    py <- ncol(y)
    what <- match.arg(what)

    if(is.null(colnames(x))) colnames(x) <- paste("V", 1:ncol(x), sep="")
    if(is.null(colnames(y))) colnames(y) <- paste("V", 1:ncol(y), sep="")

    if(what=="paired") {
        if(py != px) stop('what="paired", but ncol(x) != ncol(y) [', px, ' != ', py, ']')
        result <- corr_betw_matrices_paired(x, y)
        names(result) <- colnames(x)
    }

    else if(what=="bestright") {
        result <- corr_betw_matrices_unpaired_bestright(x, y)
        result <- as.data.frame(result)
        colnames(result) <- c("corr", "yindex")
        result$yindex <- as.integer(result$yindex)
        rownames(result) <- colnames(x)
        result <- cbind(result, ycol=colnames(y)[result[,2]], stringsAsFactors=FALSE)
    }
    else if(what=="bestpairs") {
        result <- corr_betw_matrices_unpaired_bestpairs(x, y, corr_threshold)
        result <- as.data.frame(result)
        colnames(result) <- c("corr", "xindex", "yindex")
        result$xindex <- as.integer(result$xindex)
        result$yindex <- as.integer(result$yindex)
        result <- cbind(result,
                     xcol=colnames(x)[result[,2]],
                     ycol=colnames(y)[result[,3]],
                     stringsAsFactors=FALSE)
    }
    else {
        result <- corr_betw_matrices_unpaired_all(x, y)
        dimnames(result) <- list(colnames(x), colnames(y))
    }

    result
}
