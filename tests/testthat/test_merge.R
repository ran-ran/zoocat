context('test merge.zoocat:')

test_that('test merge retain index name', {
    x <- matrix(1 : 20, nrow = 5)
    zm <- zoomly(x, order.by = 1991 : 1995,
                 colattr = data.frame(month = c(2, 3, 5, 6)))
    mg <- merge(zm, zm)
    expect_equal(indname(mg), 'year')
})