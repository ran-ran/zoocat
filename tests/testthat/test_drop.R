
context('Test zoocat object')

test_that("Drop of zoocat object", {
    x <- matrix(1 : 20, nrow = 5)
    colAttr <- data.frame(month = c(2, 3, 5, 6), name = c(rep('xxx', 3), 'yyy'))
    zc <- zoocat(x, order.by = 1991 : 1995, colattr = colAttr)
    expect_true(is.zoo(zc[, 1]))
    expect_true(is.null(dim(zc[, 1])))
    expect_equal(index(zc[, 1]), index(zc))
    expect_true(inherits(zc[, 1, drop = FALSE], 'zoocat'))
    expect_true(is.vector(zc[1, ]))
    expect_true(inherits(zc[1, , drop = FALSE], 'zoocat'))
})






