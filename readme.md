**zoocat** package is a extension of the **zoo** package. The aim of the
**zoocat** package is to provide a better manament method for
multidimensional time series. In **zoo** package, you can create a
2-dimension zoo object, in which each column stores a time series. You
can use column names to store the information of each column. The
**zoocat** package give a another method to store the attributes of each
column.

    library(zoocat)

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## Loading required package: reshape2

    ## 
    ## Attaching package: 'zoocat'

    ## The following object is masked from 'package:stats':
    ## 
    ##     cor

    mat <- matrix(round(rnorm(24), 2), ncol = 4)
    ctable <- data.frame(treatment = factor(rep(c('a', 'b'), 2), levels = c('a', 'b')), 
                         site = factor(rep(c('s1', 's2'), each = 2), levels = c('s1', 's2')))
    zc <- zoocat(mat, order.by = 2011 : 2014, colattr = ctable, index.name = 'year')
    print(zc)

    ## A zoocat object with:
    ## - [column attribute fields]: treatment, site
    ## - [index variable]: year
    ## - [data]:
    ##       a_s1  b_s1 a_s2  b_s2
    ## 2011  0.10 -0.92 0.56  1.26
    ## 2012 -1.79  0.94 0.20 -0.37
    ## 2013  0.48 -0.49 0.28 -0.20
    ## 2014  1.95  0.61 1.27  1.87

    cattr(zc)

    ##   treatment site
    ## 1         a   s1
    ## 2         b   s1
    ## 3         a   s2
    ## 4         b   s2

    zc2 <- zc
    cattr(zc2) <- data.frame(cattr(zc), observer = 'Jack')
    print(zc2)

    ## A zoocat object with:
    ## - [column attribute fields]: treatment, site, observer
    ## - [index variable]: year
    ## - [data]:
    ##      a_s1_Jack b_s1_Jack a_s2_Jack b_s2_Jack
    ## 2011      0.10     -0.92      0.56      1.26
    ## 2012     -1.79      0.94      0.20     -0.37
    ## 2013      0.48     -0.49      0.28     -0.20
    ## 2014      1.95      0.61      1.27      1.87

    indname(zc2)

    ## [1] "year"

    indname(zc2) <- 'time'
    indname(zc2)

    ## [1] "time"

    zc2 <- zc + 10
    cattr(zc2) <- data.frame(site = 's3', added = rep(TRUE, 4))
    zc.merge <- cbind(zc, zc2)
    cattr(zc.merge)

    ##   treatment site added
    ## 1         a   s1    NA
    ## 2         b   s1    NA
    ## 3         a   s2    NA
    ## 4         b   s2    NA
    ## 5      <NA>   s3  TRUE
    ## 6      <NA>   s3  TRUE
    ## 7      <NA>   s3  TRUE
    ## 8      <NA>   s3  TRUE

    df.melt <- melt(zc)
    head(df.melt)

    ##   year treatment site value
    ## 1 2011         a   s1  0.10
    ## 2 2011         b   s1 -0.92
    ## 3 2011         a   s2  0.56
    ## 4 2011         b   s2  1.26
    ## 5 2012         a   s1 -1.79
    ## 6 2012         b   s1  0.94

    cast2zoocat(df.melt, index.var = 'year', value.var = 'value')

    ## A zoocat object with:
    ## - [column attribute fields]: treatment, site
    ## - [index variable]: year
    ## - [data]:
    ##       a_s1 a_s2  b_s1  b_s2
    ## 2011  0.10 0.56 -0.92  1.26
    ## 2012 -1.79 0.20  0.94 -0.37
    ## 2013  0.48 0.28 -0.49 -0.20
    ## 2014  1.95 1.27  0.61  1.87

    reset_index_var(zc, index.var = 'treatment')

    ## A zoocat object with:
    ## - [column attribute fields]: year, site
    ## - [index variable]: treatment
    ## - [data]:
    ##   2011_s1 2011_s2 2012_s1 2012_s2 2013_s1 2013_s2 2014_s1 2014_s2
    ## a    0.10    0.56   -1.79    0.20    0.48    0.28    1.95    1.27
    ## b   -0.92    1.26    0.94   -0.37   -0.49   -0.20    0.61    1.87
