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


test_that("fscalev (in c++) works", {

    set.seed(20171201)

    x <- rnorm(24, 20, 5)
    expect_equivalent(fscalev(x), scale(x))

    # a couple of NAs
    x[2] <- x[24] <- NA
    expect_equivalent(fscalev(x), scale(x))

    # all but one NA
    x[-3] <- NA
    expect_equal(fscalev(x), x)

    # all NAs
    x[3] <- NA
    expect_equal(fscalev(x), x)

})
