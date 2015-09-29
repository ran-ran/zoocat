
test_that('Subsetting of zoocat object', {
    x <- matrix(1 : 6, nrow = 3)
    colAttr <- data.frame(month = c(2, 3), variable = c('x', 'y'))
    zc <- zoocat(x, order.by = 1991 : 1993, colattr = colAttr)
    expect_is(zc[, 1], 'zoo')
    expect_identical(index(zc[, 1]), 1991 : 1993)
    expect_identical(is.vector(zc[2, ]), TRUE)
    expect_identical(names(zc[2, ]), c('2_x', '3_y'))
    expect_is(zc[1:2, 1:2], 'zoocat')
    expect_identical(coredata(zc[1:2, 1:2]), 
                     coredata(zc)[1:2, 1:2])
    expect_is(zc[, '2_x'], 'zoo')
    expect_identical(zc[, '2_x'], zc[, 1])
    expect_identical(zc[, '3_y'], zc[, 2])
}
)


test_that('Subsetting of zoomly object', {
    x <- matrix(1 : 6, nrow = 3)
    zm <- zoomly(x, year = 1991 : 1993, month = c(2, 3))
    expect_is(zm[, 1], 'zoo')
    expect_identical(index(zm[, 1]), as.numeric(1991 : 1993))
    expect_identical(is.vector(zm[2, ]), TRUE)
    expect_identical(names(zm[2, ]), c('Feb', 'Mar'))
    expect_is(zm[1:2, 1:2], 'zoomly')
    expect_identical(coredata(zm[1:2, 1:2]), 
                     coredata(zm)[1:2, 1:2])
    expect_is(zm[, 'Feb'], 'zoo')
    expect_identical(zm[, 'Feb'], zm[, 1])
    expect_identical(zm[, 'Mar'], zm[, 2])
}
)



