context('Test melt and cast')

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
        expect_identical(coredata(valNow), zcm[i, 'value'])
    }
})

test_that('Melt of zoomly object with NA', {
    x <- matrix(c(1 : 13, rep(NA, 7)), nrow = 5)
    md <- zoomly(x, year = 1991 : 1995, month = 3 : 6)
    mdm <- melt(md, na.rm = FALSE)
    expect_equal(nrow(mdm), 20)
    mdm <- melt(md, na.rm = TRUE)
    expect_equal(nrow(mdm), 13)
    for (i in 1 : nrow(mdm)) {
        colNow <- which(mon(md) == mdm[i, 'month'])
        valNow <- md[yr(md) == mdm[i, 'year'], colNow]
        names(valNow) <- NULL
        expect_identical(coredata(valNow), mdm[i, 'value'])
    }
})


test_that('cast2zoocat, the index is Date class', {
    ind <- as.Date(12450:12455)
    mat <- matrix(rnorm(24), ncol = 4)
    ctable <- data.frame(variable = rep(c('a', 'b'), each = 2), 
                         site = rep(c('s1', 's2'), 2))
    zc <- zoocat(mat, order.by = as.Date(12450:12455), colattr = ctable)
    df.melt <- melt(zc)
    zc2 <- cast2zoocat(df.melt, index.var = 'index', value.var = 'value')
    # expect_identical(zc, zc2) # should be noted
    expect_identical(cattr(zc), cattr(zc2))
    expect_identical(index(zc), index(zc2))
    expect_identical(coredata(zc), coredata(zc2))
})


test_that('Cast and melt of zoomlyList', {
    ym <- as.yearmon(2000 + seq(0, 23)/12)
    zooobj <- zoo(matrix(1:48, nrow = 24), order.by = ym)
    colnames(zooobj) <- c('x', 'y')
    mdl <- cast2zoomly(zooobj)
    mdl.melt <- melt(mdl, ret = 'zoo')
    expect_identical(zooobj, mdl.melt)
    
    df <- melt(mdl)
    mdl <- cast2zoomly(df, value.var = 'value', 
                        variable.var = 'variable')
    mdl.melt <- melt(mdl) 
    expect_identical(df, mdl.melt)
})

