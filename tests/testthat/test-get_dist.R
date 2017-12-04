context("get_self() etc.")

set.seed(20171204)
n <- 5
m <- 7
d <- matrix(runif(n*m, 0, 10), nrow=n, ncol=m)
dimnames(d) <- list(LETTERS[1:n], sample(LETTERS[3:(m+2)]))


test_that("get_self(), etc. work", {

    expect_equal(get_self(d),
                 c(A=NA, B=NA,
                   C=d["C","C"], D=d["D", "D"], E=d["E", "E"],
                   I=NA, H=NA, F=NA, G=NA))


    # get_best() and # which_best()
    expect_equal(get_best(d, "row"),
                 c(A=d["A","C"], B=d["B","E"], C=d["C","C"], D=d["D","I"], E=d["E","D"],
                   I=NA, H=NA, F=NA, G=NA))

    expect_equal(which_best(d, "row"),
                 c(A="C", B="E", C="C", D="I", E="D", I=NA, H=NA, F=NA, G=NA))

    expect_equal(get_best(d, "row", FALSE),
                 c(A=d["A","G"], B=d["B","F"], C=d["C","D"], D=d["D","D"], E=d["E","F"],
                   I=NA, H=NA, F=NA, G=NA))

    expect_equal(which_best(d, "row", FALSE),
                 c(A="G", B="F", C="D", D="D", E="F", I=NA, H=NA, F=NA, G=NA))

    expect_equal(get_best(d, "col"),
                 c(A=NA, B=NA, C=d["C","C"], D=d["B","D"], E=d["B","E"], I=d["D","I"],
                   H=d["B","H"], F=d["C","F"], G=d["C","G"]))

    expect_equal(which_best(d, "col"),
                 c(A=NA, B=NA, C="C", D="B", E="B", I="D", H="B", F="C", G="C"))

    expect_equal(get_best(d, "col", FALSE),
                 c(A=NA, B=NA, C=d["D","C"], D=d["A","D"], E=d["A","E"],
                   I=d["E","I"], H=d["A","H"], F=d["E","F"], G=d["A","G"]))

    expect_equal(which_best(d, "col", FALSE),
                 c(A=NA, B=NA, C="D", D="A", E="A", I="E", H="A", F="E", G="A"))

    # get_2ndbest() and # which_2ndbest()
    expect_equal(get_2ndbest(d, "row"),
                 c(A=d["A","I"], B=d["B","C"], C=d["C","E"], D=d["D","E"], E=d["E","E"],
                   I=NA, H=NA, F=NA, G=NA))

    expect_equal(which_2ndbest(d, "row"),
                 c(A="I", B="C", C="E", D="E", E="E", I=NA, H=NA, F=NA, G=NA))

    expect_equal(get_2ndbest(d, "row", FALSE),
                 c(A=d["A","D"], B=d["B","H"], C=d["C","I"], D=d["D","F"], E=d["E","I"],
                   I=NA, H=NA, F=NA, G=NA))

    expect_equal(which_2ndbest(d, "row", FALSE),
                 c(A="D", B="H", C="I", D="F", E="I", I=NA, H=NA, F=NA, G=NA))

    expect_equal(get_2ndbest(d, "col"),
                 c(A=NA, B=NA, C=d["B","C"], D=d["E","D"], E=d["C","E"], I=d["B","I"],
                   H=d["C","H"], F=d["A","F"], G=d["B","G"]))

    expect_equal(which_2ndbest(d, "col"),
                 c(A=NA, B=NA, C="B", D="E", E="C", I="B", H="C", F="A", G="B"))

    expect_equal(get_2ndbest(d, "col", FALSE),
                 c(A=NA, B=NA, C=d["E","C"], D=d["D","D"], E=d["E","E"],
                   I=d["C","I"], H=d["E","H"], F=d["B","F"], G=d["D","G"]))

    expect_equal(which_2ndbest(d, "col", FALSE),
                 c(A=NA, B=NA, C="E", D="D", E="E", I="C", H="E", F="B", G="D"))


    expected <- d
    for(i in LETTERS[3:5]) expected[i,i] <- NA
    expect_equal(get_nonself(d), expected)

})
