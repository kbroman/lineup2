#' Plot histograms of self-self and self-nonself distances
#'
#' Plot histograms of self-self and self-nonself distances
#'
#' @param d A distance matrix
#' @param rug If TRUE, use [graphics::rug()] to plot tick marks at the
#' observed values, below the histograms.
#' @param breaks Histogram breaks (default is to use 100 intervals)
#' @param xlabel Label on x-axes (e.g., "similarity" vs "distance")
#'
#' @return None.
#'
#' @details We use the `mfrow` arg for [graphics::par()] to make a
#' two-panel figure.
#'
#' @seealso [get_self()], [get_nonself()]
#'
#' @importFrom graphics hist rug par
#' @export
hist_self_nonself <-
    function(d, breaks=NULL, rug=TRUE, xlabel="distance")
{
    if(is.null(breaks)) breaks <- 101
    if(is.numeric(breaks) && length(breaks)==1) {
        r <- range(d, na.rm=TRUE)
        breaks <- seq(r[1], r[2], length.out=breaks)
    }

    old_mfrow <- par("mfrow")
    on.exit(par(mfrow=old_mfrow))
    par(mfrow=c(2,1), las=1)

    self <- get_self(d)
    nonself <- get_nonself(d)

    hist(self, breaks=breaks, xlab=paste("self-self", xlabel), main="Self-self")
    if(rug) rug(self)

    hist(nonself, breaks=breaks, xlab=paste("self-nonself", xlabel), main="Self-nonself")
    if(rug) rug(nonself)
}
