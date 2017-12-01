context("Distance between matrices")

test_that("dist_betw_matrices() works", {

    set.seed(20171130)
    n <- 5
    p <- 10
    x <- matrix(rnorm(n*p), nrow=n)
    expect_equivalent(dist_betw_matrices(x, x), as.matrix(dist(x)))

    rownames(x) <- LETTERS[1:n]
    expect_equal(dist_betw_matrices(x, x), as.matrix(dist(x)))


    m <- 3
    y <- matrix(rnorm(m*p), nrow=m)
    rownames(y) <- letters[1:m]

    expect_error(dist_betw_matrices(x, y[,1:5]))

    expect_equal(dist_betw_matrices(x, y), as.matrix(dist(rbind(x,y)))[1:n, n + 1:m])

})
