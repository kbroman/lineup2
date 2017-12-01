context("Standardize columns of a matrix")

test_that("fscale (in c++) works", {

    set.seed(20171201)

    x <- matrix(rnorm(3*8, 20, 5), ncol=3)
    expect_equivalent(fscale(x), scale(x))

    x[1,2] <- x[2,1] <- x[5,3] <-NA
    result <- fscale(x)
    expect_equivalent(result, scale(x))

    x[1,2] <- x[2,1] <- x[5,3] <- Inf
    expect_equal(fscale(x), result)

    x[,1] <- NA
    x[-3,2] <- NA
    result[,1:2] <- x[,1:2]
    expect_equal(fscale(x), result)
})
