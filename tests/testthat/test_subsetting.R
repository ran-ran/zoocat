
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


test_that('Subsetting of mlydata object', {
    x <- matrix(1 : 6, nrow = 3)
    md <- mlydata(x, year = 1991 : 1993, month = c(2, 3))
    expect_is(md[, 1], 'zoo')
    expect_identical(index(md[, 1]), as.numeric(1991 : 1993))
    expect_identical(is.vector(md[2, ]), TRUE)
    expect_identical(names(md[2, ]), c('Feb', 'Mar'))
    expect_is(md[1:2, 1:2], 'mlydata')
    expect_identical(coredata(md[1:2, 1:2]), 
                     coredata(md)[1:2, 1:2])
    expect_is(md[, 'Feb'], 'zoo')
    expect_identical(md[, 'Feb'], md[, 1])
    expect_identical(md[, 'Mar'], md[, 2])
}
)



