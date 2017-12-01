#' Distance between rows of two matrices
#'
#' Calculate the distances between the rows of one matrix and the rows of a second matrix.
#'
#' @md
#'
#' @param x A numeric matrix.
#' @param y A second numeric matrix, with the same number of columns as `x`.
#'
#' @return If `x` is `n` by `p` and `y` is `m` by `p`, then the result
#'     is a `n` by `m` matrix whose (i,j)th element is the distance
#'     between the ith row of `x` and the jth row of `y`.
#'
#' @export
dist_betw_matrices <-
    function(x,y)
{
    if(!is.matrix(x)) x <- as.matrix(x)
    if(!is.matrix(y)) y <- as.matrix(y)

    if(ncol(x) != ncol(y))
        stop("x and y should have the same number of columns")

    result <- .dist_betw_matrices(x,y)
    dimnames(result) <- list(rownames(x), rownames(y))

    result
}
