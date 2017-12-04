#' Get self-nonself distances
#'
#' Return the distance matrix with all self-self distances replaced
#' with NAs (and so just containing the self-self distances).
#'
#' @md
#'
#' @param d A distance matrix
#'
#' @return The input distance matrix with all self-self distances
#' replaced with NAs.
#'
#' @seealso [get_self()], [get_best()], [get_2ndbest()]
#'
#' @export
get_nonself <-
    function(d)
{
    rn <- rownames(d)
    cn <- colnames(d)
    if(is.null(rn) || is.null(cn))
        stop("Input matrix must have both row and column names")

    # distinct individuals
    common <- rn[rn %in% cn]

    # omit self-self distances
    for(ind in common) d[ind,ind] <- NA

    d
}
