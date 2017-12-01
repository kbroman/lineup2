#' Distance between columns of two matrices
#'
#' Calculate the distances between the columns of one matrix and the columns of a second matrix.
#'
#' @md
#'
#' @param x A numeric matrix.
#' @param y A second numeric matrix, with the same number of rows as `x`.
#' @param distance Indicates whether to use Euclidean distance
#'     (`"rmsd"` for root mean square difference) or the mean absolute
#'     difference (`"mad"`).
#'
#' @return If `x` is `p` by `n` and `y` is `p` by `m`, then the result
#'     is a `n` by `m` matrix whose (i,j)th element is the distance
#'     between the ith column of `x` and the jth column of `y`.
#'
#' @export
dist_betw_matrices <-
    function(x,y, distance=c("rmsd", "mad"))
{
    distance <- match.arg(distance)

    if(!is.matrix(x)) x <- as.matrix(x)
    if(!is.matrix(y)) y <- as.matrix(y)

    if(ncol(x) != ncol(y))
        stop("x and y should have the same number of columns")

    # the C++ functions need the transpose of x and y
    if(distance=="rmsd") {
        result <- rmsd_betw_matrices(t(x), t(y))
    } else {
        result <- mad_betw_matrices(t(x), t(y))
    }

    dimnames(result) <- list(rownames(x), rownames(y))

    result
}
