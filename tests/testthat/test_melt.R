

test_that('Melt of zoocat object with NA', {
    x <- matrix(c(1 : 14, rep(NA, 6)), nrow = 5)
    colAttr <- data.frame(month = c(2, 3, 5, 6), name = c(rep('xxx', 3), 'yyy'))
    zc <- zoocat(x, order.by = 1991 : 1995, colattr = colAttr)
    zcm <- melt(zc, na.rm = FALSE)
    expect_equal(nrow(zcm), 20)
    zcm <- melt(zc, na.rm = TRUE)
    expect_equal(nrow(zcm), 14)
    for (i in 1 : nrow(zcm)) {
        colNow <- which(cattr(zc)[, 'month'] == zcm[i, 'month'] &
            cattr(zc)[, 'name'] == zcm[i, 'name'])
        valNow <- zc[index(zc) == zcm[i, 'index'], colNow]
        names(valNow) <- NULL
        expect_identical(valNow, zcm[i, 'value'])
    }
})

test_that('Melt of mlydata object with NA', {
    x <- matrix(c(1 : 13, rep(NA, 7)), nrow = 5)
    md <- mlydata(x, year = 1991 : 1995, month = 3 : 6)
    mdm <- melt(md, na.rm = FALSE)
    expect_equal(nrow(mdm), 20)
    mdm <- melt(md, na.rm = TRUE)
    expect_equal(nrow(mdm), 13)
    for (i in 1 : nrow(mdm)) {
        colNow <- which(mon(md) == mdm[i, 'month'])
        valNow <- md[yr(md) == mdm[i, 'year'], colNow]
        names(valNow) <- NULL
        expect_identical(valNow, mdm[i, 'value'])
    }
})




