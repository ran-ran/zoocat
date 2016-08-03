For using **zoocat** package, first load it:

    library(zoocat)

Construct a **zoocat** object
=============================

    mat <- matrix(round(rnorm(24), 2), ncol = 4)
    ctable <- data.frame(treatment = factor(rep(c('a', 'b'), 2), levels = c('a', 'b')), 
                         site = factor(rep(c('s1', 's2'), each = 2), levels = c('s1', 's2')))
    zc <- zoocat(mat, order.by = 2011 : 2014, colattr = ctable, index.name = 'year')
    print(zc)

    ## A zoocat object with:
    ## - [column attribute fields]: treatment, site
    ## - [index variable]: year
    ## - [data]:
    ##       a_s1  b_s1  a_s2  b_s2
    ## 2011 -0.19  0.86 -0.26  0.11
    ## 2012 -2.07 -0.37 -2.13 -0.12
    ## 2013  1.19 -0.35  1.34  0.56
    ## 2014 -1.61  0.23 -2.15  1.00

Set and get the **cattr** table
===============================

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
    ## 2011     -0.19      0.86     -0.26      0.11
    ## 2012     -2.07     -0.37     -2.13     -0.12
    ## 2013      1.19     -0.35      1.34      0.56
    ## 2014     -1.61      0.23     -2.15      1.00

Merge by columns
================

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

Melt and cast
=============

    df.melt <- melt(zc)
    head(df.melt)

    ##   year treatment site value
    ## 1 2011         a   s1 -0.19
    ## 2 2011         b   s1  0.86
    ## 3 2011         a   s2 -0.26
    ## 4 2011         b   s2  0.11
    ## 5 2012         a   s1 -2.07
    ## 6 2012         b   s1 -0.37

    cast2zoocat(df.melt, index.var = 'year', value.var = 'value')

    ## A zoocat object with:
    ## - [column attribute fields]: treatment, site
    ## - [index variable]: year
    ## - [data]:
    ##       a_s1  a_s2  b_s1  b_s2
    ## 2011 -0.19 -0.26  0.86  0.11
    ## 2012 -2.07 -2.13 -0.37 -0.12
    ## 2013  1.19  1.34 -0.35  0.56
    ## 2014 -1.61 -2.15  0.23  1.00

Reset the index variable
========================

    reset_index_var(zc, index.var = 'treatment')

    ## A zoocat object with:
    ## - [column attribute fields]: year, site
    ## - [index variable]: treatment
    ## - [data]:
    ##   2011_s1 2011_s2 2012_s1 2012_s2 2013_s1 2013_s2 2014_s1 2014_s2
    ## a   -0.19   -0.26   -2.07   -2.13    1.19    1.34   -1.61   -2.15
    ## b    0.86    0.11   -0.37   -0.12   -0.35    0.56    0.23    1.00
