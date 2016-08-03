**zoocat** package is an extension package of the **zoo** package. The
aim of this package is:

-   Manipulate multivariate time serie conveniently.
-   Convert between **zoocat** object and data frame.
-   Represent multi-dimensional data by two-dimensional format.

For using **zoocat** package, first load it:

    library(zoocat)

Construct a **zoocat** object
=============================

Using following codes to construct a **zoocat** object.

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
    ## 2011 -0.90  1.99  1.23 -0.12
    ## 2012 -1.03 -1.15  1.28 -0.95
    ## 2013 -1.72 -0.72  1.74 -1.02
    ## 2014  0.72 -0.87 -0.85 -0.49

Note that all information of each column of the **zoocat** object is
stored in a data frame, which is as the argument "colattr" for function
"zoocat". This data frame is called column attribute table (**cattr**
table).

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
    ## 2011     -0.90      1.99      1.23     -0.12
    ## 2012     -1.03     -1.15      1.28     -0.95
    ## 2013     -1.72     -0.72      1.74     -1.02
    ## 2014      0.72     -0.87     -0.85     -0.49

Merge by columns
================

    zc2 <- zc + 10
    cattr(zc2) <- data.frame(site = 's3', added = rep(TRUE, 4))
    zc.merge <- cbind(zc, zc2)
    print(zc.merge)

    ## A zoocat object with:
    ## - [column attribute fields]: treatment, site, added
    ## - [index variable]: year
    ## - [data]:
    ##      a_s1_NA b_s1_NA a_s2_NA b_s2_NA NA_s3_TRUE NA_s3_TRUE NA_s3_TRUE
    ## 2011   -0.90    1.99    1.23   -0.12       9.10      11.99      11.23
    ## 2012   -1.03   -1.15    1.28   -0.95       8.97       8.85      11.28
    ## 2013   -1.72   -0.72    1.74   -1.02       8.28       9.28      11.74
    ## 2014    0.72   -0.87   -0.85   -0.49      10.72       9.13       9.15
    ##      NA_s3_TRUE
    ## 2011       9.88
    ## 2012       9.05
    ## 2013       8.98
    ## 2014       9.51

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

Similar with package **reshape2**, you can melt and cast between data
frame and **zoocat**.

    df.melt <- melt(zc)
    head(df.melt)

    ##   year treatment site value
    ## 1 2011         a   s1 -0.90
    ## 2 2011         b   s1  1.99
    ## 3 2011         a   s2  1.23
    ## 4 2011         b   s2 -0.12
    ## 5 2012         a   s1 -1.03
    ## 6 2012         b   s1 -1.15

    cast2zoocat(df.melt, index.var = 'year', value.var = 'value')

    ## A zoocat object with:
    ## - [column attribute fields]: treatment, site
    ## - [index variable]: year
    ## - [data]:
    ##       a_s1  a_s2  b_s1  b_s2
    ## 2011 -0.90  1.23  1.99 -0.12
    ## 2012 -1.03  1.28 -1.15 -0.95
    ## 2013 -1.72  1.74 -0.72 -1.02
    ## 2014  0.72 -0.85 -0.87 -0.49

Reset the index variable
========================

    reset_index_var(zc, index.var = 'treatment')

    ## A zoocat object with:
    ## - [column attribute fields]: year, site
    ## - [index variable]: treatment
    ## - [data]:
    ##   2011_s1 2011_s2 2012_s1 2012_s2 2013_s1 2013_s2 2014_s1 2014_s2
    ## a   -0.90    1.23   -1.03    1.28   -1.72    1.74    0.72   -0.85
    ## b    1.99   -0.12   -1.15   -0.95   -0.72   -1.02   -0.87   -0.49
