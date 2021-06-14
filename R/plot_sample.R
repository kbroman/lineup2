#' Plot the distances for a given sample
#'
#' Plot the distances for a given sample, highlighting itself and the closest sample
#'
#' @param d A distance or similarity matrix
#' @param sample Sample ID (in row or column names)
#' @param dimension Whether to look at the row or column
#' @param get_min If TRUE, get the minimum (for a distance matrix);
#'     if FALSE, get the maximum (for a similarity matrix)
#' @param add_labels If TRUE, label the individual sample and the optimal sample
#' @param ... Passed to `points()`
#'
#' @return None.
#'
#' @examples
#' # align rows in the provided dataset, lineup2ex
#' aligned <- align_matrix_rows(lineup2ex$gastroc, lineup2ex$islet)
#' # find correlated columns
#' selected_genes <- (corr_betw_matrices(aligned[[1]], aligned[[2]], "paired") > 0.75)
#' # calculate correlation between rows
#' similarity <- corr_betw_matrices(t(lineup2ex$gastroc[,selected_genes]),
#'                                  t(lineup2ex$islet[,selected_genes]), "all")
#'
#' plot_sample(similarity, "Mouse3659", get_min=FALSE)
#' plot_sample(similarity, "Mouse3655", "column", get_min=FALSE)
#'
#' @export
#' @importFrom graphics points text par
#' @importFrom stats median

plot_sample <-
    function(d, sample, dimension=c("row", "column"), get_min=TRUE,
             add_labels=TRUE, ...)
{
    dimension <- match.arg(dimension)
    if(!((dimension == "row" && sample %in% rownames(d)) ||
         (dimension == "column" && sample %in% colnames(d)))) {
        stop("sample ", sample, " not found in ", dimension, " names")
    }


    # internal function
    plot_sample_internal <-
        function(d, sample, dimension, get_min, add_labels,
                 pch=21, bg="lightblue", las=1, main=paste("Sample", sample),
                 xlab=ifelse(dimension=="row", "Column", "Row"),
                 ylab=ifelse(get_min, "Distance", "Similarity"),
                 ylim=NULL, xlim=NULL,
                 bg_self="green3", bg_opt="violetred", ...)
        {
            # values to plot
            if(dimension=="row") {
                y <- d[sample,]
            } else {
                y <- d[,sample]
            }
            x <- seq_along(y)

            # self sample to highlight in green
            y_self <- y[sample]
            x_self <- which(names(y)==sample)

            # optimal sample to highlight in pink
            if(get_min) {
                the_opt <- min(y, na.rm=TRUE)
            } else {
                the_opt <- max(y, na.rm=TRUE)
            }
            x_opt <- which(!is.na(y) & y==the_opt)
            y_opt <- y[x_opt]

            if(is.null(xlim)) xlim <- range(x, na.rm=TRUE)
            if(is.null(ylim)) ylim <- range(y, na.rm=TRUE)

            plot(x, y, xlab=xlab, ylab=ylab, type="n", main=main,
                 xlim=xlim, ylim=ylim, las=las)
            points(x, y, pch=pch, bg=bg, ...)
            points(x_opt, y_opt, pch=pch, bg=bg_opt, ...)
            points(x_self, y_self, pch=pch, bg=bg_self, ...)

            labx <- c(x_self, x_opt)
            laby <- c(y_self, y_opt)
            lab <- c(sample, names(x_opt))

            # make unique
            ulab <- unique(lab)
            wh <- match(ulab, lab)
            labx <- labx[wh]
            laby <- laby[wh]
            lab <- lab[wh]

            u <- par("usr")[1:2]
            on_left <- (labx < mean(u))
            dx <- diff(u)*0.01
            if(any(on_left)) {
                text(labx[on_left]+dx, laby[on_left], lab[on_left], adj=c(0, 0.5))
            }
            if(any(!on_left)) {
                text(labx[!on_left]-dx, laby[!on_left], lab[!on_left], adj=c(1, 0.5))
            }

        }

    plot_sample_internal(d, sample, dimension, get_min, add_labels, ...)
}
