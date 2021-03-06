% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/which_2ndbest.R
\name{which_2ndbest}
\alias{which_2ndbest}
\title{Determine which individual has 2nd-smallest distance to each individual}
\usage{
which_2ndbest(d, dimension = c("row", "column"), get_min = TRUE)
}
\arguments{
\item{d}{A distance matrix}

\item{dimension}{Whether to get the 2nd-best by row or by column}

\item{get_min}{If TRUE, get the 2nd-minimum; if FALSE, get the 2nd-maximum}
}
\value{
A vector with \strong{all} distinct individuals, with the
character string labels for the individuals giving the
2nd-smallest (or largest) value by row or column. We include
all individuals so that the results are aligned with the
results of \code{\link[=get_self]{get_self()}}.
}
\description{
For each individual represented in a distance matrix, find the
individual giving the 2nd-smallest entry (with NAs for individuals
present in only the rows or only the columns).
}
\examples{
# align rows in the provided dataset, lineup2ex
aligned <- align_matrix_rows(lineup2ex$gastroc, lineup2ex$islet)
# find correlated columns
selected_genes <- (corr_betw_matrices(aligned[[1]], aligned[[2]], "paired") > 0.75)
# calculate correlation between rows
similarity <- corr_betw_matrices(t(lineup2ex$gastroc[,selected_genes]),
                                 t(lineup2ex$islet[,selected_genes]), "all")
# which sample gives second-biggest value by row
secbest_byrow <- which_2ndbest(similarity, get_min=FALSE)

# which sample gives second-biggest value by column
secbest_bycol <- which_2ndbest(similarity, get_min=FALSE, dimension="column")

}
\seealso{
\code{\link[=get_2ndbest]{get_2ndbest()}}, \code{\link[=get_self]{get_self()}}, \code{\link[=get_best]{get_best()}}, \code{\link[=which_best]{which_best()}}
}
