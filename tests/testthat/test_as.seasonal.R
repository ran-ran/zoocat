context('Test as.seasonal.zoomly')

test_that('test as.seasonal.zoomly',  {
    x <- matrix(1 : 36, nrow = 3, byrow = TRUE)
    zm <- zoomly(x, year = 1991 : 1993, month = 1 : 12)
    zs1 <- as.seasonal(zm)
    zs2 <- as.seasonal(zm, DJF.first = FALSE)
    expect_equal(colnames(zs1), c('DJF', 'MAM', 'JJA', 'SON'))
    expect_equal(colnames(zs2), c('MAM', 'JJA', 'SON', 'DJF'))
    expect_equal(as.vector(t(coredata(zs1))),
                 c(NA, seq(from = 4, to = 34, by = 3)))
    expect_equal(as.vector(t(coredata(zs2))),
                 c(seq(from = 4, to = 34, by = 3), NA))
}
)