
context('test zoomly')

test_that('test problom of zoomly on frequency', {
    zc <- zoocat(matrix(1 : 10, ncol = 2), colattr = data.frame(a = 1:2, month = 1:2))
    zm <- as.zoomly(zc)
    expect_identical(coredata(diff(zc)), coredata(diff(zm)))
})