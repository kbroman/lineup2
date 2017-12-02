# align the rows of two matrices
align_matrix_rows <-
    function(x, y)
{
    rnx <- rownames(x)
    rny <- rownames(y)

    if(is.null(rnx) || is.null(rny)) return(list(x=x,y=y))

    if(length(unique(rnx)) != length(rnx) ||
       length(unique(rny)) != length(rny))
        stop("The row names aren't distinct.")

    common <- rnx[rnx %in% rny]

    # return just the rows that are in common
    list(x=x[common,,drop=FALSE], y=y[common,,drop=FALSE])
}

# align the columns of two matrices
align_matrix_cols <-
    function(x, y)
{
    cnx <- colnames(x)
    cny <- colnames(y)

    if(is.null(cnx) || is.null(cny)) return(list(x=x,y=y))

    if(length(unique(cnx)) != length(cnx) ||
       length(unique(cny)) != length(cny))
        stop("The column names aren't distinct.")

    common <- cnx[cnx %in% cny]

    # return just the cols that are in common
    list(x=x[,common,drop=FALSE], y=y[,common,drop=FALSE])
}
