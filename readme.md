true

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

```R
    mat <- matrix(round(rnorm(24), 2), ncol = 4)
    ctable <- data.frame(treatment = factor(rep(c('a', 'b'), 2), levels = c('a', 'b')), 
                         site = factor(rep(c('s1', 's2'), each = 2), levels = c('s1', 's2')))
    zc <- zoocat(mat, order.by = 2011 : 2014, colattr = ctable, index.name = 'year')
    print(zc)
```
    #> A zoocat object with:
    #> - [column attribute fields]: treatment, site
    #> - [index variable]: year
    #> - [data]:
    #>       a_s1  b_s1  a_s2  b_s2
    #> 2011  0.25 -1.60 -2.11 -0.34
    #> 2012 -1.26 -0.75  0.08  1.36
    #> 2013  0.86 -0.57  0.87  1.16
    #> 2014 -0.25 -1.07  0.03  0.20

Note that all information of each column of the **zoocat** object is
stored in a data frame, which is as the argument "colattr" for function
"zoocat". This data frame is called column attribute table (**cattr**
table).

Set and get the **cattr** table
===============================

    cattr(zc)
    #>   treatment site
    #> 1         a   s1
    #> 2         b   s1
    #> 3         a   s2
    #> 4         b   s2
    zc2 <- zc
    cattr(zc2) <- data.frame(cattr(zc), observer = 'Jack')
    print(zc2)
    #> A zoocat object with:
    #> - [column attribute fields]: treatment, site, observer
    #> - [index variable]: year
    #> - [data]:
    #>      a_s1_Jack b_s1_Jack a_s2_Jack b_s2_Jack
    #> 2011      0.25     -1.60     -2.11     -0.34
    #> 2012     -1.26     -0.75      0.08      1.36
    #> 2013      0.86     -0.57      0.87      1.16
    #> 2014     -0.25     -1.07      0.03      0.20

Merge by columns
================

    zc2 <- zc + 10
    cattr(zc2) <- data.frame(site = 's3', added = rep(TRUE, 4))
    zc.merge <- cbind(zc, zc2)
    print(zc.merge)
    #> A zoocat object with:
    #> - [column attribute fields]: treatment, site, added
    #> - [index variable]: year
    #> - [data]:
    #>      a_s1_NA b_s1_NA a_s2_NA b_s2_NA NA_s3_TRUE NA_s3_TRUE NA_s3_TRUE
    #> 2011    0.25   -1.60   -2.11   -0.34      10.25       8.40       7.89
    #> 2012   -1.26   -0.75    0.08    1.36       8.74       9.25      10.08
    #> 2013    0.86   -0.57    0.87    1.16      10.86       9.43      10.87
    #> 2014   -0.25   -1.07    0.03    0.20       9.75       8.93      10.03
    #>      NA_s3_TRUE
    #> 2011       9.66
    #> 2012      11.36
    #> 2013      11.16
    #> 2014      10.20
    cattr(zc.merge)
    #>   treatment site added
    #> 1         a   s1    NA
    #> 2         b   s1    NA
    #> 3         a   s2    NA
    #> 4         b   s2    NA
    #> 5      <NA>   s3  TRUE
    #> 6      <NA>   s3  TRUE
    #> 7      <NA>   s3  TRUE
    #> 8      <NA>   s3  TRUE

Melt and cast
=============

Similar with package **reshape2**, you can melt and cast between data
frame and **zoocat**.

    df.melt <- melt(zc)
    head(df.melt)
    #>   year treatment site value
    #> 1 2011         a   s1  0.25
    #> 2 2011         b   s1 -1.60
    #> 3 2011         a   s2 -2.11
    #> 4 2011         b   s2 -0.34
    #> 5 2012         a   s1 -1.26
    #> 6 2012         b   s1 -0.75

    cast2zoocat(df.melt, index.var = 'year', value.var = 'value')
    #> A zoocat object with:
    #> - [column attribute fields]: treatment, site
    #> - [index variable]: year
    #> - [data]:
    #>       a_s1  a_s2  b_s1  b_s2
    #> 2011  0.25 -2.11 -1.60 -0.34
    #> 2012 -1.26  0.08 -0.75  1.36
    #> 2013  0.86  0.87 -0.57  1.16
    #> 2014 -0.25  0.03 -1.07  0.20

Reset the index variable
========================

    reset_index_var(zc, index.var = 'treatment')
    #> A zoocat object with:
    #> - [column attribute fields]: year, site
    #> - [index variable]: treatment
    #> - [data]:
    #>   2011_s1 2011_s2 2012_s1 2012_s2 2013_s1 2013_s2 2014_s1 2014_s2
    #> a    0.25   -2.11   -1.26    0.08    0.86    0.87   -0.25    0.03
    #> b   -1.60   -0.34   -0.75    1.36   -0.57    1.16   -1.07    0.20
