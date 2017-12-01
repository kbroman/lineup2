context("Distance between matrices")

test_that("dist_betw_matrices() works", {

    set.seed(20171130)
    n <- 5
    x <- matrix(rnorm(n*10), nrow=n)
    expect_equivalent(dist_betw_matrices(x, x), as.matrix(dist(x)))

    rownames(x) <- LETTERS[1:n]
    expect_equal(dist_betw_matrices(x, x), as.matrix(dist(x)))

})
