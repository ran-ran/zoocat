context('Test coercion between classes')

test_that('as.zoo for zoomly and zoocat', {
    x <- matrix(1 : 20, nrow = 5)
    md <- zoomly(x, year = 1991 : 1995, month = c(2, 3, 5, 6))
    zo <- as.zoo(md)
    expect_equal(class(zo), c('zooreg', 'zoo'))
    zo <- as.zoo(md, add.colname = FALSE)
    expect_equal(colnames(zo), NULL)
    zc <- as.zoocat(md)
    zo <- as.zoo(zc)
    expect_equal(class(zo), c('zooreg', 'zoo'))
    zo <- as.zoo(zc, add.colname = FALSE)
    expect_equal(colnames(zo), NULL)
}
)